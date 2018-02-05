{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

-- | Setup consisting of a few threads to send and receive packets to and from
-- the euphoria api using a websocket connection.

module EuphApi.Threads (
  -- * Connecting to euphoria
    Connection
  , euphApp
  , getEvent
  -- * API functions
  , pingReply
  , nick
  , send
  -- * Exception
  , EuphException(..)
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Trans.State
import           Data.Aeson                as A
import qualified Data.ByteString.Lazy      as BS
import qualified Data.HashMap.Strict       as HM
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import           Data.Time
import           Data.Time.Clock.POSIX
import qualified Network.WebSockets        as WS

import qualified EuphApi.Types             as E

type PacketID = T.Text
type Reply = Either EuphException
data ReplyMVar = forall r . (FromJSON r) => ReplyMVar (MVar (Reply r))

type SendQueue  = TBQueue Send
type RecvQueue  = TBQueue Recv
type EventQueue = TBQueue (Maybe Event) -- 'Nothing' ends the event stream
type LockedFlag = TVar Bool

data Connection = Connection LockedFlag SendQueue EventQueue

-- | Read one 'Event' from the 'Connection'.
--
-- Returns 'Nothing' once when the 'Connection' stops.
-- After that, any further calls of getEvent on the same connection
-- will block indefinitely.
getEvent :: Connection -> IO (Maybe Event)
getEvent (Connection locked _ qEvent) = undefined locked qEvent

-- | A @Network.Websockets.'WS.ClientApp'@ creating a 'Connection'.
euphApp :: WS.ClientApp Connection
euphApp con = do
  sendQueue  <- atomically $ newTBQueue 10
  recvQueue  <- atomically $ newTBQueue 10
  eventQueue <- atomically $ newTBQueue 10
  locked     <- atomically $ newTVar False
  let euphCon = Connection locked sendQueue eventQueue
  void $ forkIO $ recvThread euphCon recvQueue con
  return euphCon

{-
 - Fetch thread
 -}

fetchThread :: RecvQueue -> WS.Connection -> IO ()
fetchThread qRecv con = void $ handle handleException $ forever $ do
  message <- WS.receiveData con
  void $ atomically $ writeTBQueue qRecv (RPacket message)
  where
    handleException (WS.CloseRequest _ _) = atomically $ writeTBQueue qRecv RDisconnected
    handleException WS.ConnectionClosed   = atomically $ writeTBQueue qRecv RDisconnected
    handleException _                     = fetchThread qRecv con

{-
 - Send thread
 -}

-- Prepare a single packet for sending.
-- Doesn't actually do any IO. The IO monad part is left in for ease of use.
preparePacket :: (ToJSON p) => T.Text -> p -> StateT Integer IO (BS.ByteString, PacketID)
preparePacket packetType packetData = do
  packetNr <- get
  put $ packetNr + 1
  let packetID = T.pack $ show packetNr
      bytestr  = encode . Object . HM.fromList $
        [ ("id",   A.String packetID)
        , ("type", A.String packetType)
        , ("data", toJSON packetData)
        ]
  return (bytestr, packetID)

readWhileOpen :: Connection -> STM (Maybe Send)
readWhileOpen (Connection locked qSend _) = do
  isLocked <- readTVar locked
  if isLocked
    then return Nothing
    else Just <$> readTBQueue qSend

sendThread :: Connection -> RecvQueue -> WS.Connection -> StateT Integer IO ()
sendThread euphCon qRecv con = do
  item <- liftIO $ atomically $ readWhileOpen euphCon
  case item of
    Nothing ->
      return ()

    Just SDisconnect ->
      liftIO $ WS.sendClose con ("Bye." :: T.Text)

    Just (SNoReply packetType packetData) -> do
      (packet, _) <- preparePacket packetType packetData
      liftIO $ WS.sendTextData con packet
      continue <- liftIO $ sendSafely packet
      when continue $
        sendThread euphCon qRecv con

    Just (SReply packetType packetData reply) -> do
      (packet, packetID) <- preparePacket packetType packetData
      void $ liftIO $ atomically $ writeTBQueue qRecv (RReply packetID reply)
      continue <- liftIO $ sendSafely packet
      when continue $
        sendThread euphCon qRecv con
  where
    sendSafely packet = (WS.sendTextData con packet >> return True) `catch` handleException
    handleException (WS.CloseRequest _ _) = return False
    handleException WS.ConnectionClosed   = return False
    handleException _                     = return True

{-
 - Receive thread
 -}

data PacketInfo = PacketInfo
  { infoPacketID    :: Maybe PacketID
  , infoServerError :: Maybe T.Text
  } deriving (Show)

instance FromJSON PacketInfo where
  parseJSON = withObject "packet" $ \o -> do
    infoPacketID <- o .:? "id"
    infoServerError <- o .:? "error"
    return PacketInfo{..}

-- TODO: Swap for HashMap?
type Awaiting = M.Map T.Text ReplyMVar

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust m f = maybe (return ()) f m

processPacket :: EventQueue -> BS.ByteString -> StateT Awaiting IO ()
processPacket qEvent bs = do
  -- First, deal with event channel events.
  case A.decode bs of
    Nothing    -> return ()
    Just event -> liftIO $ atomically $ writeTBQueue qEvent (Just event)
  -- Then, deal with replies.
  -- Find out whether there is actually any dealing with replies to do...
  replies <- get
  let result = do -- Maybe monad
        PacketInfo{..} <- A.decode bs
        replyID <- infoPacketID
        replyMVar <- M.lookup replyID replies
        return (replyID, replyMVar, infoServerError)
  -- ... and then write the appropriate result into the MVar.
  whenJust result $ \(replyID, ReplyMVar var, serverError) -> do
    modify (M.delete replyID)
    case serverError of
      Just e -> liftIO $ putMVar var (Left (EuphServerError e))
      Nothing ->
        case A.decode bs of
          Nothing -> liftIO $ putMVar var (Left EuphParse)
          Just r  -> liftIO $ putMVar var (Right r)

processRecv :: RecvQueue -> EventQueue -> StateT Awaiting IO ()
processRecv qRecv qEvent = do
  recv <- liftIO $ atomically $ readTBQueue qRecv
  case recv of
    RReply packetID replyMVar -> do
      modify (M.insert packetID replyMVar)
      processRecv qRecv qEvent

    RPacket bs -> do
      processPacket qEvent bs
      processRecv qRecv qEvent

    RDisconnected -> return ()

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

cleanupRecv :: RecvQueue -> IO ()
cleanupRecv qRecv = do
  recvs <- atomically $ emptyTBQueue qRecv
  forM_ recvs $ \case
    RReply _ (ReplyMVar var) -> putMVar var (Left EuphDisconnected)
    _                        -> return ()

recvThread :: Connection -> RecvQueue -> WS.Connection -> IO ()
recvThread euphCon@(Connection locked qSend qEvent) qRecv con = do
  tFetch <- async $ fetchThread qRecv con
  tSend  <- async $ evalStateT (sendThread euphCon qRecv con) 0
  waitingReplies <- execStateT (processRecv qRecv qEvent) M.empty
  atomically $ writeTVar locked True
  wait tFetch
  wait tSend
  cleanupWaiting waitingReplies
  cleanupSend qSend
  cleanupRecv qRecv
  atomically $ writeTBQueue qEvent Nothing

{-
 - API functions
 -}

sendPacket :: (ToJSON p, FromJSON r) => Connection -> T.Text -> p -> IO r
sendPacket (Connection locked qSend _) packetType packetData = do
  var <- newEmptyMVar
  let packet = SReply packetType packetData (ReplyMVar var)
  atomically $ do
    isLocked <- readTVar locked
    if isLocked
      then throwSTM EuphClosed
      else writeTBQueue qSend packet
  result <- readMVar var
  case result of
    Left f  -> throw f
    Right r -> return r

sendPacketNoReply :: (ToJSON p) => Connection -> T.Text -> p -> IO ()
sendPacketNoReply (Connection locked qSend _) packetType packetData = atomically $ do
  let packet = SNoReply packetType packetData
  isLocked <- readTVar locked
  if isLocked
    then throwSTM EuphClosed
    else writeTBQueue qSend packet

pingReply :: Connection -> UTCTime -> IO ()
pingReply euphCon pingReplyCommandTime =
  sendPacketNoReply euphCon "ping-reply" PingReplyCommand{..}

nick :: Connection -> T.Text -> IO (E.Nick, E.Nick)
nick euphCon nickCommandName = do
  NickReply{..} <- sendPacket euphCon "nick" NickCommand{..}
  return (nickReplyFrom, nickReplyTo)

send :: Connection -> Maybe E.Snowflake -> T.Text -> IO E.Message
send euphCon sendCommandParent sendCommandContent = do
  SendReply{..} <- sendPacket euphCon "send" SendCommand{..}
  return sendReplyMessage



{-
 - Some types
 -}

-- | The ways in which getting a reply from the server can fail.
--
-- An EuphException may be raised by any function in the API functions section.
--
-- TODO: link to section if possible
data EuphException = EuphClosed
                     -- ^ Could not send message because connection was closed.
                   | EuphDisconnected
                     -- ^ Disconnected from server while waiting for the reply.
                   | EuphServerError T.Text
                     -- ^ The server replied with an error.
                   | EuphParse
                     -- ^ Could not parse the server's reply correctly.

instance Show EuphException where
  show EuphClosed          = "Connection already closed"
  show EuphDisconnected    = "Disconnected from server"
  show (EuphServerError t) = "Server error: " ++ T.unpack t
  show EuphParse           = "Parsing failed"

instance Exception EuphException

data Recv = RDisconnected
          | RPacket BS.ByteString
          | RReply PacketID ReplyMVar

data Send = SDisconnect
          | forall p . (ToJSON p) => SNoReply T.Text p -- packet type and contents
          | forall p . (ToJSON p) => SReply T.Text p ReplyMVar

data Event
  = BounceEvent (Maybe T.Text) (Maybe [T.Text])
    -- ^ A 'BounceEvent' indicates that access to a room is denied.
    --
    -- @'BounceEvent' (Maybe reason) (Maybe [authOption])@
  | DisconnectEvent T.Text
    -- ^ A 'DisconnectEvent' indicates that the session is being closed.
    -- The client will subsequently be disconnected.
    --
    -- If the disconnect reason is "authentication changed",
    -- the client should immediately reconnect.
    --
    -- @'DisconnectEvent' reason@
  | HelloEvent E.SessionView Bool T.Text
    -- ^ A 'HelloEvent' is sent by the server to the client
    -- when a session is started.
    -- It includes information about the client's authentication
    -- and associated identity.
    --
    -- @'HelloEvent' session roomIsPrivate version@
  | JoinEvent E.SessionView
    -- ^ A 'JoinEvent' indicates a session just joined the room.
    --
    -- @'JoinEvent' session@
  | NetworkEvent T.Text T.Text
    -- ^ A 'NetworkEvent' indicates some server-side event
    -- that impacts the presence of sessions in a room.
    --
    -- If the network event type is "partition",
    -- then this should be treated as a 'PartEvent' for all sessions
    -- connected to the same server id/era combo.
    --
    -- @'NetworkEvent' server_id server_era@
  | NickEvent E.Nick E.Nick
    -- ^ A 'NickEvent' announces a nick change by another session in the room.
    --
    -- @'NickEvent' from to@
  | EditMessageEvent E.Message
    -- ^ An 'EditMessageEvent' indicates that a message in the room
    -- has been modified or deleted.
    -- If the client offers a user interface and the indicated message
    -- is currently displayed, it should update its display accordingly.
    --
    -- The event packet includes a snapshot of the message post-edit.
    --
    -- @'EditMessageEvent' editedMessage@
  | PartEvent E.SessionView
    -- ^ A 'PartEvent' indicates a session just disconnected from the room.
    --
    -- @'PartEvent' session@
  | PingEvent UTCTime UTCTime
    -- ^ A 'PingEvent' represents a server-to-client ping.
    -- The client should send back a 'pingReply' with the same value
    -- for the time field as soon as possible (or risk disconnection).
    --
    -- @'PingEvent' time next@
  | SendEvent E.Message
    -- ^ A 'SendEvent' indicates a message received by the room
    -- from another session.
    --
    -- @'SendEvent' message@
  | SnapshotEvent T.Text [E.SessionView] [E.Message] (Maybe E.Nick)
    -- ^ A 'SnapshotEvent' indicates that a session has
    -- successfully joined a room.
    -- It also offers a snapshot of the room’s state and recent history.
    --
    -- @'SnapshotEvent' version listing log (Maybe nick)@

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
        NickEvent <$> o .: "from" <*> o .: "to"
      pEditMessageEvent v = EditMessageEvent <$> parseJSON v
      pPartEvent v = PartEvent <$> parseJSON v
      pPingEvent = withObject "PingEvent" $ \o ->
        PingEvent <$> o .: "time" <*> o .: "next"
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
