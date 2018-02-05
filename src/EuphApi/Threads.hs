{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

-- | Setup consisting of a few threads to send and receive packets to and from
-- the euphoria api using a websocket connection.
--
-- @
-- m: main thread
-- r: recvThread
-- f: fetchThread
-- s: sendThread
--
-- On creation:
--  m: Create WS connection (or do this in r?)
--  m: Create channels
--  m: Start recvThread with all necessary info
--  r: Start fetchThread and sendThread using async
--  m: Return SendChan and EventChan
--
-- On disconnect:
--  s: close connection (optional)
--  f: detect exception
--  f: RDisconnected -> RecvChan
--  f: *stops*
--  r: RecvChan -> RDisconnected
--  r: EDisconnected -> EventChan
--  r: close SendChan
--  s: *stops*
--  r: wait for f and s to stop
--  r: clean up SendChan
--  r: clean up RecvChan
--  r: clean up response list
--  r: EStopped -> EventChan
--  r: *stops*
--  -> All MVars are dealt with
--
--                        ↓
--                        │
--                    (SendChan)
--                        │
-- ┌─────────────────────╴│╶──────┐
-- │                      │       │
-- │ (WS.Connection)      │       │
-- │        │             │       │
-- │  [fetchThread]  [sendThread] │
-- │        │             │       │
-- │        └──────┬──────┘       │
-- │               │              │
-- │           (RecvChan)         │
-- │               │              │
-- │          [recvThread]        │
-- │               │              │
-- └──────────────╴│╶─────────────┘
--                 │
--             (EventChan)
--                 │
--                 ↓
-- @

module EuphApi.Threads (
  -- * Connecting to euphoria
    EuphConnection
  , euphClient
  , getEvents
  -- * API functions
  , pingReply
  , nick
  , send
  -- * Events and replies
  , EuphException(..)
  , EuphEvent(..)
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe

import           Control.Concurrent.Async
import           Control.Monad.Trans.State
import           Data.Aeson                as A
import qualified Data.ByteString.Lazy      as BS
import qualified Data.HashMap.Strict       as HM
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import           Data.Time
import           Data.Time.Clock.POSIX
import qualified Network.WebSockets        as WS

import qualified EuphApi.CloseableChan     as E
import qualified EuphApi.Types             as E

-- Some useful type aliases
type PacketID = T.Text
type Reply = Either EuphException
data ReplyMVar = forall r . (FromJSON r) => ReplyMVar (MVar (Reply r))

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

sendPacket :: (ToJSON p, FromJSON r) => EuphConnection -> T.Text -> p -> IO r
sendPacket (EuphConnection chan _) packetType packetData = do
  var <- newEmptyMVar
  let packet = SReply packetType packetData (ReplyMVar var)
  done <- E.writeChan chan packet
  case done of
    Nothing -> throw EuphClosed
    Just () -> do
      result <- readMVar var
      case result of
        Left f  -> throw f
        Right r -> return r

sendPacketNoReply :: (ToJSON p) => EuphConnection -> T.Text -> p -> IO ()
sendPacketNoReply (EuphConnection chan _) packetType packetData = do
  let packet = SNoReply packetType packetData
  done <- E.writeChan chan packet
  case done of
    Nothing -> throw EuphClosed
    Just () -> return ()

{-
 - API functions
 -}

-- TODO: Add proper documentation
pingReply :: EuphConnection -> UTCTime -> IO ()
pingReply econ pingReplyCommandTime =
  sendPacketNoReply econ "ping-reply" PingReplyCommand{..}

-- TODO: Add proper documentation
nick :: EuphConnection -> T.Text -> IO (E.Nick, E.Nick)
nick econ nickCommandName = do
  NickReply{..} <- sendPacket econ "nick" NickCommand{..}
  return (nickReplyFrom, nickReplyTo)

-- TODO: Add proper documentation
send :: EuphConnection -> Maybe E.Snowflake -> T.Text -> IO E.Message
send econ sendCommandParent sendCommandContent = do
  SendReply{..} <- sendPacket econ "send" SendCommand{..}
  return sendReplyMessage

{-
 - Commands and replies
 -}

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

{-
 - Events
 -}

-- | Represents <http://api.euphoria.io/#asynchronous-events>.
--
-- These events may be sent from the server to the client at any time.
data EuphEvent = BounceEvent (Maybe T.Text) (Maybe [T.Text])
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

instance FromJSON EuphEvent where
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

{-
 - Channels
 -}

type RecvChan = E.CloseableChan Recv
data Recv = RDisconnected
          | RPacket BS.ByteString
          | RReply PacketID ReplyMVar

type SendChan = E.CloseableChan Send
data Send = SDisconnect
          | forall p . (ToJSON p) => SNoReply T.Text p -- packet type and contents
          | forall p . (ToJSON p) => SReply T.Text p ReplyMVar

type EventChan = Chan Event
type Event = Maybe EuphEvent

{-
 - Fetch thread
 -}

fetchThread :: RecvChan -> WS.Connection -> IO ()
fetchThread cRecv con = handle handleException $ forever $ do
  message <- WS.receiveData con
  void $ E.writeChan cRecv (RPacket message) -- will never be closed while thread running
  where
    handleException (WS.CloseRequest _ _) = void $ E.writeChan cRecv RDisconnected
    handleException WS.ConnectionClosed   = void $ E.writeChan cRecv RDisconnected
    handleException _                     = fetchThread cRecv con

{-
 - Send thread
-}

-- Prepare a single packet for sending
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

sendThread :: SendChan -> RecvChan -> WS.Connection -> StateT Integer IO ()
sendThread cSend cRecv con = do
  item <- liftIO $ E.readChan cSend
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
        sendThread cSend cRecv con

    Just (SReply packetType packetData reply) -> do
      (packet, packetID) <- preparePacket packetType packetData
      void $ liftIO $ E.writeChan cRecv (RReply packetID reply)
      continue <- liftIO $ sendSafely packet
      when continue $
        sendThread cSend cRecv con
  where
    sendSafely packet = (WS.sendTextData con packet >> return True) `catch` handleException
    handleException (WS.CloseRequest _ _) = return False
    handleException WS.ConnectionClosed   = return False
    handleException _                     = return True

{-
 - RecvThread
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

processRecv :: RecvChan -> EventChan -> Awaiting -> IO Awaiting
processRecv cRecv cEvent replies = do
  recv <- E.readChan cRecv
  case recv of
    Just (RReply packetID replyMVar) -> do
      let newReplies = M.insert packetID replyMVar replies
      processRecv cRecv cEvent newReplies

    Just (RPacket bs) -> do
      newReplies <- processPacket cEvent bs replies
      processRecv cRecv cEvent newReplies

    _ -> return replies

processPacket :: EventChan -> BS.ByteString -> Awaiting -> IO Awaiting
processPacket cEvent bs replies = do
  -- First, deal with event channel events.
  case A.decode bs of
    Nothing    -> return ()
    Just event -> writeChan cEvent (Just event)
  -- Then, deal with replies.
  fromMaybe (return replies) $ do
    PacketInfo{..} <- A.decode bs
    replyID <- infoPacketID
    (ReplyMVar var) <- M.lookup replyID replies
    let newReplies = M.delete replyID replies
    case infoServerError of
      Nothing -> do
        reply <- A.decode bs
        return $ newReplies <$ putMVar var (Right reply)
      Just e ->
        return $ newReplies <$ putMVar var (Left (EuphServerError e))

cleanupWaiting :: Awaiting -> IO ()
cleanupWaiting replies =
  forM_ replies $ \(ReplyMVar var) -> putMVar var (Left EuphDisconnected)

cleanupSend :: SendChan -> IO ()
cleanupSend cSend = do
  sends <- E.emptyChan cSend
  forM_ sends $ \case
    SReply _ _ (ReplyMVar var) -> putMVar var (Left EuphDisconnected)
    _                          -> return ()

cleanupRecv :: RecvChan -> IO ()
cleanupRecv cRecv = do
  recvs <- E.emptyChan cRecv
  forM_ recvs $ \case
    RReply _ (ReplyMVar var) -> putMVar var (Left EuphDisconnected)
    _                        -> return ()

recvThread :: SendChan -> RecvChan -> EventChan -> WS.Connection -> IO ()
recvThread cSend cRecv cEvent con = do
  tFetch <- async $ fetchThread cRecv con
  tSend  <- async $ evalStateT (sendThread cSend cRecv con) 0
  waitingReplies <- processRecv cRecv cEvent M.empty
  E.closeChan cSend
  wait tFetch
  wait tSend
  cleanupWaiting waitingReplies
  cleanupSend cSend
  cleanupRecv cRecv
  writeChan cEvent Nothing

{-
 - Startup
 -}

-- TODO: Add proper documentation
data EuphConnection = EuphConnection SendChan EventChan

-- TODO: Add proper documentation
getEvents :: EuphConnection -> IO [EuphEvent]
getEvents (EuphConnection _ cEvent) = do
  events <- getChanContents cEvent
  return $ catMaybes $ takeWhile isJust events

-- TODO: Add proper documentation
euphClient :: WS.ClientApp EuphConnection
euphClient con = do
  sendChan  <- E.newOpenChan
  recvChan  <- E.newOpenChan
  eventChan <- newChan
  void $ forkIO $ recvThread sendChan recvChan eventChan con
  return $ EuphConnection sendChan eventChan
