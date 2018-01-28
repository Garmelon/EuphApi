{-# LANGUAGE ExistentialQuantification #-}
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
--  m: Create WS connection
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
--  r: close SendChan
--  s: *stops*
--  r: wait for f and s to stop
--  r: clean up SendChan
--  r: clean up RecvChan
--  r: clean up response list
--  r: EventStopped -> EventChan
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
  -- * Events and replies
    Failure(..)
  , Event(..)
  , EuphEvent(..)
  -- * API functions
  , pingReply
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Aeson                as A
import qualified Data.ByteString.Lazy      as BS
import qualified Data.HashMap.Strict       as HM
import qualified Data.Text                 as T
import           Data.Time
import           Data.Time.Clock.POSIX
import qualified EuphApi.CloseableChan     as E
import qualified EuphApi.Types             as E
import qualified Network.WebSockets        as WS

-- Some useful type aliases
type PacketID = T.Text
type Reply = Either Failure

-- | The ways in which getting a reply from the server can fail.
data Failure = FailClosed       -- ^ Could not send message because connection was closed.
             | FailDisconnect   -- ^ Disconnected from server while waiting for the reply.
             | FailError T.Text -- ^ The server replied with an error.
             | FailParse        -- ^ Could not parse the server's reply correctly.

{-
 - Commands and replies
 -}

(.?=) :: (ToJSON v, KeyValue kv) => T.Text -> Maybe v -> [kv]
k .?= (Just v) = [k .= v]
k .?= Nothing  = []

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
  , sendCommandParent  :: Maybe PacketID
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
                 -- If the disconnect reason is "authentication changed", the client should immediately reconnect.
                 --
                 -- @'DisconnectEvent' reason@
               | HelloEvent E.SessionView Bool T.Text
                 -- ^ A 'HelloEvent' is sent by the server to the client when a session is started.
                 -- It includes information about the client's authentication and associated identity.
                 --
                 -- @'HelloEvent' session roomIsPrivate version@
               | JoinEvent E.SessionView
                 -- ^ A 'JoinEvent' indicates a session just joined the room.
                 --
                 -- @'JoinEvent' session@
               | NetworkEvent T.Text T.Text
                 -- ^ A 'NetworkEvent' indicates some server-side event that impacts the presence of sessions in a room.
                 --
                 -- If the network event type is "partition", then this should be treated as a 'PartEvent' for all sessions connected to the same server id/era combo.
                 --
                 -- @'NetworkEvent' server_id server_era@
               | NickEvent E.Nick E.Nick
                 -- ^ A 'NickEvent' announces a nick change by another session in the room.
                 --
                 -- @'NickEvent' from to@
               | EditMessageEvent E.Message
                 -- ^ An 'EditMessageEvent' indicates that a message in the room has been modified or deleted.
                 -- If the client offers a user interface and the indicated message is currently displayed, it should update its display accordingly.
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
                 -- The client should send back a 'pingReply' with the same value for the time field as soon as possible (or risk disconnection).
                 --
                 -- @'PingEvent' time next@
               | SendEvent E.Message
                 -- ^ A 'SendEvent' indicates a message received by the room from another session.
                 --
                 -- @'SendEvent' message@
               | SnapshotEvent T.Text [E.SessionView] [E.Message] (Maybe E.Nick)
                 -- ^ A 'SnapshotEvent' indicates that a session has successfully joined a room.
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
 - API functions
 -}

pingReply :: SendChan -> UTCTime -> IO (Reply ())
pingReply = undefined

{-
pingReply chan time = do
  let obj    = object $ ["time" .= utcTimeToPOSIXSeconds time]
      packet = packetOfType "ping-reply" obj
  sent <- liftIO $ E.writeChan chan $ SNoReply packet
  case sent of
    Nothing -> return $ Left FailClosed
    Just _  -> return $ Right ()

nick :: SendChan -> T.Text -> IO (Reply (T.Text, T.Text))
nick chan newNick = do
  let obj    = object $ ["name" .= newNick]
      packet = packetOfType "nick" obj
  var <- liftIO newEmptyMVar
  sent <- liftIO $ E.writeChan chan $ SReply packet var
  case sent of
    Nothing -> return $ Left FailClosed
    Just _  -> do
      reply <- readMVar var
      case reply of
        Left f              -> return $ Left f
        Right NickReply{..} -> return $ Right (nickReplyFrom, nickReplyTo)

send :: SendChan -> T.Text -> IO (Reply E.Message)
send chan content = do
  let obj    = object $ ["content" .= content]
      packet = packetOfType "send" obj
  var <- liftIO newEmptyMVar
  sent <- liftIO $ E.writeChan chan $ SReply packet var
  case sent of
    Nothing -> return $ Left FailClosed
    Just _  -> do
      reply <- readMVar var
      return $ sendReplyMessage <$> reply

reply :: SendChan -> PacketID -> T.Text -> IO (Reply E.Message)
reply chan parent content = do
  let obj    = object $ ["content" .= content, "parent" .= parent]
      packet = packetOfType "send" obj
  var <- liftIO newEmptyMVar
  sent <- liftIO $ E.writeChan chan $ SReply packet var
  case sent of
    Nothing -> return $ Left FailClosed
    Just _  -> do
      reply <- readMVar var
      return $ sendReplyMessage <$> reply
-}

{-
 - Channels
 -}

type RecvChan = E.CloseableChan Recv
data Recv = RDisconnected
          | RPacket BS.ByteString
          | forall a . (FromJSON a) => RReply PacketID (MVar (Reply a))

type SendChan = E.CloseableChan Send
data Send = SDisconnect
          | forall p . (ToJSON p) => SNoReply T.Text p -- packet type and contents
          | forall p r . (ToJSON p, FromJSON r) => SReply T.Text p (MVar (Reply r))

type EventChan e = E.CloseableChan (Event e)
data Event e = EDisconnected
             | EStopped
             | EEuphEvent EuphEvent
             | ECustomEvent e

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

type SendState = StateT Integer IO

-- Prepare a single packet for sending
preparePacket :: (ToJSON p) => T.Text -> p -> SendState (BS.ByteString, PacketID)
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

sendThread :: SendChan -> RecvChan -> WS.Connection -> SendState ()
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
      liftIO $ E.writeChan cRecv $ RReply packetID reply
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

-- TODO
