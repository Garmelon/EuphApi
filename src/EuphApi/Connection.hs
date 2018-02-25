{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

-- | Setup consisting of a few threads to send and receive packets to and from
-- the euphoria api using a websocket connection.
--
-- Objects of type 'Connection' represent a connection to a server.
-- @Connection@s can't be reused, so to reconnect to a server, a new @Connection@ object
-- must be used.
--
-- To connect to a server, a new connection must be created using the
-- 'startEuphConnection' command.
--
-- == Example
--
-- > import qualified EuphApi.Connection as E
-- >
-- > main = do
-- >   putStrLn "> Connecting to euphoria.io, &test"
-- >   con <- E.startEuphConnection "euphoria.io" "test"
-- >   printEvents con
-- >   where
-- >     printEvents con = do
-- >       event <- E.getEvent con
-- >       case event of
-- >         ConnectionFailed -> putStrLn "> Could not connect. Are you sure that room exists?"
-- >         Disconnected     -> putStrLn "> Connection closed. Bye!"
-- >         EuphEvent e      -> print e

module EuphApi.Connection (
  -- * Connecting to euphoria
    Connection
  , startEuphConnection
  , getEvent
  -- * API functions
  , disconnect
  , pingReply
  -- ** Session commands
  , auth
  , ping
  -- ** Chat room commands
  , getMessage
  , messageLog
  , nick
  -- pmInitiate
  , send
  , who
  -- * Events and Exceptions
  , EuphException(..)
  , EventType(..)
  , Event(..)
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Trans.State
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy      as BS
import qualified Data.HashMap.Strict       as HM
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import           Data.Time
import           Data.Time.Clock.POSIX
import qualified Network.WebSockets        as WS
import qualified Wuss                      as WSS

import qualified EuphApi.Types             as E

type PacketID = T.Text
type Reply = Either EuphException
data ReplyMVar = forall r . (FromJSON r) => ReplyMVar (MVar (Reply r))

type SendQueue  = TBQueue Send
type EventQueue = TBQueue EventType
type LockedFlag = TVar Bool

-- | A connection to a room on euphoria.
data Connection = Connection LockedFlag SendQueue EventQueue

-- | Read one event from the 'Connection'.
--
-- If the @Connection@ could not connect, this returns 'ConnectionFailed' once.
-- If the @Connection@ stops, this returns 'Disconnected' once.
--
-- After either a @ConnectionFailed@ or a @Disconnected@, any further calls
-- of @getEvent@ on the same @Connection@ will block indefinitely.
getEvent :: Connection -> IO EventType
getEvent (Connection _ _ qEvent) = atomically $ readTBQueue qEvent

-- | Creates a new 'Connection' that will attempt to connect to a given room:
--
-- > con <- startEuphConnection server room
startEuphConnection :: String -> String -> IO Connection
startEuphConnection host room = do
  sendQueue  <- atomically $ newTBQueue 10
  eventQueue <- atomically $ newTBQueue 10
  locked     <- atomically $ newTVar False
  let euphCon = Connection locked sendQueue eventQueue
  void
    $ forkIO
    $ handle (handleException eventQueue)
    $ WSS.runSecureClient host 443 ("/room/" ++ room ++ "/ws")
    $ recvClient euphCon
  return euphCon
  where
    handleException :: EventQueue -> WS.HandshakeException -> IO ()
    handleException qEvent _ = atomically $ writeTBQueue qEvent ConnectionFailed

{-
 - Send thread
 -}

-- Prepare a single packet for sending.
-- Doesn't actually do any IO. The IO monad part is left in for ease of use.
preparePacket :: (ToJSON p) => T.Text -> p -> StateT Integer IO (BS.ByteString, PacketID)
preparePacket packetType packetData = do
  packetNr <- get
  modify (+1)
  let packetID = T.pack $ show packetNr
      bytestr  = encode . Object . HM.fromList $
        [ ("id",   String packetID)
        , ("type", String packetType)
        , ("data", toJSON packetData)
        ]
  return (bytestr, packetID)

readWhileOpen :: Connection -> STM (Maybe Send)
readWhileOpen (Connection locked qSend _) = do
  isLocked <- readTVar locked
  if isLocked
    then return Nothing
    else Just <$> readTBQueue qSend

sendThread :: RecvInfo -> StateT Integer IO ()
sendThread info@RecvInfo{..} = do
  item <- liftIO $ atomically $ readWhileOpen recvEuphCon
  case item of
    Nothing ->
      return ()

    Just SDisconnect ->
      liftIO $ WS.sendClose recvCon ("Bye. -EuphApi" :: T.Text)

    Just (SNoReply packetType packetData) -> do
      (packet, _) <- preparePacket packetType packetData
      continue <- liftIO $ sendSafely packet
      when continue $
        sendThread info

    Just (SReply packetType packetData reply) -> do
      (packet, packetID) <- preparePacket packetType packetData
      liftIO $ atomically $ modifyTVar recvWaiting (M.insert packetID reply)
      continue <- liftIO $ sendSafely packet
      when continue $
        sendThread info
  where
    sendSafely packet = (WS.sendTextData recvCon packet >> return True) `catch` handleException
    handleException (WS.CloseRequest _ _) = return False
    handleException WS.ConnectionClosed   = return False
    handleException _                     = return True
    -- TODO: Think about whether this is safe (memory leak issues etc.)

{-
 - Receive thread
 -}

data PacketInfo = PacketInfo
  { infoPacketID :: Maybe PacketID
  , infoData     :: Either T.Text Value
  } deriving (Show)

instance FromJSON PacketInfo where
  parseJSON = withObject "packet" $ \o -> do
    infoPacketID    <- o .:? "id"
    packetData <- o .:? "data"
    infoData <- case packetData of
      Nothing -> Left <$> o .: "error"
      Just d  -> return $ Right d
    return PacketInfo{..}

data RecvInfo = RecvInfo
  { recvEuphCon :: Connection
  , recvCon     :: WS.Connection
  , recvWaiting :: TVar Awaiting
  }

-- TODO: Swap for HashMap?
type Awaiting = M.Map T.Text ReplyMVar

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust m f = maybe (return ()) f m

processEvent :: BS.ByteString -> RecvInfo -> IO ()
processEvent bs RecvInfo{..} = do
  let (Connection _ _ qEvent) = recvEuphCon
  case decode bs of
    Nothing    -> return ()
    Just event -> atomically $ writeTBQueue qEvent (EuphEvent event)

processReply :: BS.ByteString -> RecvInfo -> IO ()
processReply bs RecvInfo{..} = do
  -- Figure out whether this packet is actually a reply of some sort.
  let maybeInfo = do
        PacketInfo{..} <- decode bs
        replyID        <- infoPacketID
        return (replyID, infoData)
  whenJust maybeInfo $ \(replyID, infoData) -> do
    -- Figure out whether we're waiting for that ID and find the correct MVar
    -- and remove it from the TVar if we do find it.
    maybeReplyMVar <- atomically $ do
      waiting <- readTVar recvWaiting
      case M.lookup replyID waiting of
        Nothing  -> return Nothing
        Just var -> do
          modifyTVar recvWaiting (M.delete replyID)
          return (Just var)
    whenJust maybeReplyMVar $ \(ReplyMVar var) ->
      -- We now know that the packet is a reply, and we know the MVar to send
      -- it to. Now we only need to send the correct reply through the MVar.
      case infoData of
        Left e  -> putMVar var (Left $ EuphServerError e)
        Right d ->
          case parseEither parseJSON d of
            Left e  -> putMVar var (Left $ EuphParse e bs)
            Right r -> putMVar var (Right r)

processRecv :: RecvInfo -> IO ()
processRecv info@RecvInfo{..} = handle handleException $ forever $ do
  message <- WS.receiveData recvCon -- retrieve packet from ws connection
  processEvent message info
  processReply message info
  where
    handleException (WS.CloseRequest _ _) = return ()
    handleException WS.ConnectionClosed   = return ()
    handleException _                     = processRecv info -- continue running

cleanupWaiting :: Awaiting -> IO ()
cleanupWaiting replies =
  forM_ replies $ \(ReplyMVar var) -> putMVar var (Left EuphDisconnected)

emptyTBQueue :: TBQueue a -> STM [a]
emptyTBQueue q = do
  isEmpty <- isEmptyTBQueue q
  if isEmpty
    then return []
    else do
      item <- readTBQueue q
      rest <- emptyTBQueue q
      return $ item : rest

cleanupSend :: SendQueue -> IO ()
cleanupSend qSend = do
  sends <- atomically $ emptyTBQueue qSend
  forM_ sends $ \case
    SReply _ _ (ReplyMVar var) -> putMVar var (Left EuphDisconnected)
    _                          -> return ()

recvClient :: Connection -> WS.ClientApp ()
recvClient euphCon@(Connection locked qSend qEvent) con = do
  waiting <- atomically $ newTVar M.empty
  let info = RecvInfo{recvEuphCon=euphCon, recvCon=con, recvWaiting=waiting}
  tSend  <- async $ evalStateT (sendThread info) 0
  processRecv info
  -- Stop and clean up stuff
  atomically $ writeTVar locked True
  wait tSend
  cleanupSend qSend
  atomically (readTVar waiting) >>= cleanupWaiting
  atomically $ writeTBQueue qEvent Disconnected

{-
 - API functions
 -}

writeSend :: Connection -> Send -> STM ()
writeSend (Connection locked qSend _) s = do
  isLocked <- readTVar locked
  if isLocked
    then throwSTM EuphClosed
    else writeTBQueue qSend s

sendPacket :: (ToJSON p, FromJSON r) => Connection -> T.Text -> p -> IO r
sendPacket euphCon packetType packetData = do
  var <- newEmptyMVar
  let packet = SReply packetType packetData (ReplyMVar var)
  atomically $ writeSend euphCon packet
  result <- readMVar var
  case result of
    Left f  -> throwIO f
    Right r -> return r

sendPacketNoReply :: (ToJSON p) => Connection -> T.Text -> p -> IO ()
sendPacketNoReply euphCon packetType packetData = do
  let packet = SNoReply packetType packetData
  atomically $ writeSend euphCon packet

-- | Close the 'Connection'. This will result in a 'Disconnected' event.
disconnect :: Connection -> IO ()
disconnect euphCon = atomically $ writeSend euphCon SDisconnect

-- | Reply to the server's 'PingEvent',
-- as described in <http://api.euphoria.io/#ping-event>.
pingReply :: Connection -> UTCTime -> IO ()
pingReply euphCon pingReplyCommandTime =
  sendPacketNoReply euphCon "ping-reply" PingReplyCommand{..}

-- | Implements <http://api.euphoria.io/#auth>.
--
-- The 'auth' command attempts to join a private room.
-- It should be sent in response to a 'BounceEvent' at the beginning of a session.
--
-- The reply reports whether the @auth@ command succeeded.
-- 'Nothing' implies success whereas @'Just' error@ reports an authentication error.
--
-- > success <- auth con passcode
auth :: Connection -> T.Text -> IO (Maybe T.Text)
auth euphCon authCommandPasscode = do
  AuthReply{..} <- sendPacket euphCon "auth" AuthCommand{..}
  return authReplySuccess

-- | Implements <http://api.euphoria.io/#ping>.
--
-- The 'ping' command initiates a client-to-server ping.
-- The server will send back a reply with the same timestamp as soon as possible.
--
-- This uses the current time as value for the @time@ field.
-- Might be useful to check whether a connection still works.
-- Could also be used for measuring the server delay.
-- As all other api functions, this will also throw an exception if the connection closed.
--
-- > ping con
ping :: Connection -> IO ()
ping euphCon = do
  pingCommandTime <- getCurrentTime
  PingReply{..} <- sendPacket euphCon "ping" PingCommand{..}
  return ()

-- | Implements <http://api.euphoria.io/#get-message>.
--
-- The 'getMessage' command retrieves the full content of a single message in the room.
--
-- > message <- getMessage con id
getMessage :: Connection -> E.Snowflake -> IO E.Message
getMessage euphCon getMessageCommandID = do
  GetMessageReply{..} <- sendPacket euphCon "get-message" GetMessageCommand{..}
  return getMessageReplyMessage

-- | Implements <http://api.euphoria.io/#log>.
--
-- The 'messageLog' command requests messages from the room’s message log.
-- This can be used to supplement the log provided by 'SnapshotEvent'
-- (for example, when scrolling back further in history).
--
-- The command returns a list of 'E.Message's from the room’s message log.
--
-- > (log, Maybe before) <- messageLog con amount (Maybe before)
messageLog :: Connection
           -> Integer
           -> Maybe E.Snowflake
           -> IO ([E.Message], Maybe E.Snowflake)
messageLog euphCon logCommandN logCommandBefore = do
  LogReply{..} <- sendPacket euphCon "log" LogCommand{..}
  return (logReplyLog, logReplyBefore)

-- | Implements <http://api.euphoria.io/#nick>.
--
-- The 'nick' command sets the name you present to the room.
-- This name applies to all messages sent during this session,
-- until the @nick@ command is called again.
--
-- The reply confirms the @nick@ command.
-- It returns the session’s former and new names
-- (the server may modify the requested nick).
--
-- > (from, to) <- nick con name
nick :: Connection -> T.Text -> IO (E.Nick, E.Nick)
nick euphCon nickCommandName = do
  NickReply{..} <- sendPacket euphCon "nick" NickCommand{..}
  return (nickReplyFrom, nickReplyTo)

-- | Implements <http://api.euphoria.io/#send>.
--
-- The 'send' command sends a message to a room.
-- The session must be successfully joined with the room.
-- This message will be broadcast to all sessions joined with the room.
--
-- If the room is private, then the message content will be encrypted
-- before it is stored and broadcast to the rest of the room.
--
-- The caller of this command will not receive the corresponding 'SendEvent',
-- but will receive the same information in the reply.
--
-- The reply returns the 'E.Message' that was sent.
-- This includes the message id ('E.Snowflake'), which was populated by the server.
--
-- > message <- send (Maybe parentID) content
send :: Connection -> Maybe E.Snowflake -> T.Text -> IO E.Message
send euphCon sendCommandParent sendCommandContent = do
  SendReply{..} <- sendPacket euphCon "send" SendCommand{..}
  return sendReplyMessage

-- | Implements <http://api.euphoria.io/#who>.
--
-- The who command returns a list of sessions currently joined in the room.
--
-- > sessions <- who
who :: Connection -> IO [E.SessionView]
who euphCon = do
  WhoReply{..} <- sendPacket euphCon "who" WhoCommand
  return whoReplyListing



{-
 - Some types
 -}

-- | The ways in which getting a reply from the server can fail.
--
-- An @EuphException@ may be raised by any function in the __API functions__ section.
data EuphException = EuphClosed
                     -- ^ Could not send message because connection was closed.
                   | EuphDisconnected
                     -- ^ Disconnected from server while waiting for the reply.
                   | EuphServerError T.Text
                     -- ^ The server replied with an error.
                   | EuphParse String BS.ByteString
                     -- ^ Could not parse the server's reply correctly.

instance Show EuphException where
  show EuphClosed           = "Connection already closed"
  show EuphDisconnected     = "Disconnected from server"
  show (EuphServerError t)  = "Server error: " ++ T.unpack t
  show (EuphParse e bs)     = "Parsing failed: " ++ e ++ " - packet was " ++ show bs

instance Exception EuphException

data Send = SDisconnect
          | forall p . (ToJSON p) => SNoReply T.Text p -- packet type and contents
          | forall p . (ToJSON p) => SReply T.Text p ReplyMVar

-- | The kinds of events that can occur in a 'Connection'.
-- For more information, see 'getEvent'.
data EventType = ConnectionFailed
                 -- ^ The @Connection@ failed to connect to that room.
                 -- When this event occurrs, no more events will occur.
               | Disconnected
                 -- ^ The @Connection@ was closed.
                 -- When this event occurrs, no more events will occur.
               | EuphEvent Event
                 -- ^ The server sent an 'Event'.
  deriving (Show)

-- | Represents <http://api.euphoria.io/#asynchronous-events>.
--
-- These events may be sent from the server to the client at any time.
data Event
  = BounceEvent (Maybe T.Text) (Maybe [T.Text])
    -- ^ A @BounceEvent@ indicates that access to a room is denied.
    --
    -- > BounceEvent (Maybe reason) (Maybe [authOption])
  | DisconnectEvent T.Text
    -- ^ A @DisconnectEvent@ indicates that the session is being closed.
    -- The client will subsequently be disconnected.
    --
    -- If the disconnect reason is "authentication changed",
    -- the client should immediately reconnect.
    --
    -- > DisconnectEvent reason
  | HelloEvent E.SessionView Bool T.Text
    -- ^ A @HelloEvent@ is sent by the server to the client
    -- when a session is started.
    -- It includes information about the client's authentication
    -- and associated identity.
    --
    -- > HelloEvent session roomIsPrivate version
  | JoinEvent E.SessionView
    -- ^ A @JoinEvent@ indicates a session just joined the room.
    --
    -- > JoinEvent session
  | NetworkEvent T.Text T.Text
    -- ^ A @NetworkEvent@ indicates some server-side event
    -- that impacts the presence of sessions in a room.
    --
    -- If the network event type is "partition",
    -- then this should be treated as a 'PartEvent' for all sessions
    -- connected to the same server id/era combo.
    --
    -- > NetworkEvent server_id server_era
  | NickEvent E.SessionID E.Nick E.Nick
    -- ^ A @NickEvent@ announces a nick change by another session in the room.
    --
    -- > NickEvent from to
  | EditMessageEvent E.Message
    -- ^ An @EditMessageEvent@ indicates that a message in the room
    -- has been modified or deleted.
    -- If the client offers a user interface and the indicated message
    -- is currently displayed, it should update its display accordingly.
    --
    -- The event packet includes a snapshot of the message post-edit.
    --
    -- > EditMessageEvent editedMessage
  | PartEvent E.SessionView
    -- ^ A @PartEvent@ indicates a session just disconnected from the room.
    --
    -- > PartEvent session
  | PingEvent UTCTime UTCTime
    -- ^ A @PingEvent@ represents a server-to-client ping.
    -- The client should send back a 'pingReply' with the same value
    -- for the time field as soon as possible (or risk disconnection).
    --
    -- > PingEvent time next
  | SendEvent E.Message
    -- ^ A @SendEvent@ indicates a message received by the room
    -- from another session.
    --
    -- > SendEvent message
  | SnapshotEvent T.Text [E.SessionView] [E.Message] (Maybe E.Nick)
    -- ^ A @SnapshotEvent@ indicates that a session has
    -- successfully joined a room.
    -- It also offers a snapshot of the room’s state and recent history.
    --
    -- > SnapshotEvent version listing log (Maybe nick)
  deriving (Show)

  -- LoginEvent -- not implemented
  -- LogoutEvent -- not implemented
  -- PMInitiateEvent -- not implemented

instance FromJSON Event where
  parseJSON = withObject "Event" $ \o -> do
    tp <- o .: "type"
    dt <- o .: "data"
    empty
      <|> (tp `is` "bounce-event"       >> pBounceEvent dt)
      <|> (tp `is` "disconnect-event"   >> pDisconnectEvent dt)
      <|> (tp `is` "hello-event"        >> pHelloEvent dt)
      <|> (tp `is` "join-event"         >> pJoinEvent dt)
      <|> (tp `is` "network-event"      >> pNetworkEvent dt)
      <|> (tp `is` "nick-event"         >> pNickEvent dt)
      <|> (tp `is` "edit-message-event" >> pEditMessageEvent dt)
      <|> (tp `is` "part-event"         >> pPartEvent dt)
      <|> (tp `is` "ping-event"         >> pPingEvent dt)
      <|> (tp `is` "send-event"         >> pSendEvent dt)
      <|> (tp `is` "snapshot-event"     >> pSnapshotEvent dt)
    where
      a `is` b = guard ((a :: T.Text) == b)
      pBounceEvent = withObject "BounceEvent" $ \o ->
        BounceEvent <$> o .:? "reason" <*> o .:? "auth_options"
      pDisconnectEvent = withObject "DisconnectEvent" $ \o ->
        DisconnectEvent <$> o .: "reason"
      pHelloEvent = withObject "HelloEvent" $ \o ->
        HelloEvent <$> o .: "session" <*> o .: "room_is_private" <*> o .: "version"
      pJoinEvent v = JoinEvent <$> parseJSON v
      pNetworkEvent = withObject "NetworkEvent" $ \o ->
        NetworkEvent <$> o .: "server_id" <*> o .: "server_era"
      pNickEvent = withObject "NickEvent" $ \o ->
        NickEvent <$> o .: "session_id" <*> o .: "from" <*> o .: "to"
      pEditMessageEvent v = EditMessageEvent <$> parseJSON v
      pPartEvent v = PartEvent <$> parseJSON v
      pPingEvent = withObject "PingEvent" $ \o ->
        PingEvent <$> (posixSecondsToUTCTime <$> o .: "time")
                  <*> (posixSecondsToUTCTime <$> o .: "next")
      pSendEvent v = SendEvent <$> parseJSON v
      pSnapshotEvent = withObject "SnapshotEvent" $ \o ->
        SnapshotEvent <$> o .: "version" <*> o .: "listing" <*> o .: "log" <*> o .:? "nick"

(.?=) :: (ToJSON v, KeyValue kv) => T.Text -> Maybe v -> [kv]
k .?= (Just v) = [k .= v]
_ .?= Nothing  = []

-- ping reply/command/whatever

newtype PingReplyCommand = PingReplyCommand
  { pingReplyCommandTime :: UTCTime
  } deriving (Show)

instance ToJSON PingReplyCommand where
  toJSON PingReplyCommand{..} =
    object ["time" .= utcTimeToPOSIXSeconds pingReplyCommandTime]

-- auth command and reply

newtype AuthCommand = AuthCommand
  { authCommandPasscode :: T.Text
  } deriving (Show)

instance ToJSON AuthCommand where
  toJSON AuthCommand{..} =
    object ["type" .= ("passcode" :: T.Text), "passcode" .= authCommandPasscode]

newtype AuthReply = AuthReply
  { authReplySuccess :: Maybe T.Text
  } deriving (Show)

instance FromJSON AuthReply where
  parseJSON = withObject "AuthReply" $ \o -> do
    success <- o .: "success"
    authReplySuccess <-
      if success
        then Just <$> o .: "reason"
        else return Nothing
    return AuthReply{..}

-- ping command and reply

newtype PingCommand = PingCommand
  { pingCommandTime :: UTCTime
  } deriving (Show)

instance ToJSON PingCommand where
  toJSON PingCommand{..} =
    let timestr = show $ utcTimeToPOSIXSeconds pingCommandTime
    in  object ["time" .= timestr]

-- TODO: Maybe always return a "time"
newtype PingReply = PingReply
  { pingReplyTime :: Maybe UTCTime
  } deriving (Show)

instance FromJSON PingReply where
  parseJSON = withObject "PingReply" $ \o -> do
    maybeTime <- o .:? "time"
    let pingReplyTime = posixSecondsToUTCTime <$> maybeTime
    return PingReply{..}

-- get-message command and reply

newtype GetMessageCommand = GetMessageCommand
  { getMessageCommandID :: E.Snowflake
  } deriving (Show)

instance ToJSON GetMessageCommand where
  toJSON GetMessageCommand{..} =
    object ["id" .= getMessageCommandID]

newtype GetMessageReply = GetMessageReply
  { getMessageReplyMessage :: E.Message
  } deriving (Show)

instance FromJSON GetMessageReply where
  parseJSON v = GetMessageReply <$> parseJSON v

-- log command and reply

data LogCommand = LogCommand
  { logCommandN      :: Integer
  , logCommandBefore :: Maybe E.Snowflake
  } deriving (Show)

instance ToJSON LogCommand where
  toJSON LogCommand{..} =
    object $ ("n" .= logCommandN) : ("before" .?= logCommandBefore)

-- TODO: Maybe omit the "before" if it's always the same as the one from the command?
-- TODO: Maybe always return a "before"?
data LogReply = LogReply
  { logReplyLog    :: [E.Message]
  , logReplyBefore :: Maybe E.Snowflake
  } deriving (Show)

instance FromJSON LogReply where
  parseJSON = withObject "LogReply" $ \o -> do
    logReplyLog    <- o .: "log"
    logReplyBefore <- o .:? "before"
    return LogReply{..}

-- nick command and reply

newtype NickCommand = NickCommand
  { nickCommandName :: T.Text
  } deriving (Show)

instance ToJSON NickCommand where
  toJSON NickCommand{..} =
    object ["name" .= nickCommandName]

data NickReply = NickReply
  { nickReplySessionID :: E.SessionID
  , nickReplyUserID    :: E.UserID
  , nickReplyFrom      :: T.Text
  , nickReplyTo        :: T.Text
  } deriving (Show)

instance FromJSON NickReply where
  parseJSON = withObject "NickReply" $ \o -> do
    nickReplySessionID <- o .: "session_id"
    nickReplyUserID    <- o .: "id"
    nickReplyFrom      <- o .: "from"
    nickReplyTo        <- o .: "to"
    return NickReply{..}

-- send command and reply

data SendCommand = SendCommand
  { sendCommandContent :: T.Text
  , sendCommandParent  :: Maybe E.Snowflake
  } deriving (Show)

instance ToJSON SendCommand where
  toJSON SendCommand{..} =
    object $ ("content" .= sendCommandContent) : ("parent" .?= sendCommandParent)

newtype SendReply = SendReply
  { sendReplyMessage :: E.Message
  } deriving (Show)

instance FromJSON SendReply where
  parseJSON v = SendReply <$> parseJSON v

-- who command and reply

data WhoCommand = WhoCommand
  deriving (Show)

instance ToJSON WhoCommand where
  toJSON WhoCommand = object []

newtype WhoReply = WhoReply
  { whoReplyListing :: [E.SessionView]
  } deriving (Show)

instance FromJSON WhoReply where
  parseJSON = withObject "WhoReply" $ \o -> do
    listing <- o .: "listing"
    return (WhoReply listing)
