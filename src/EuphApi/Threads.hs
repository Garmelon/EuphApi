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
  -- * Functions for using the api
  , send
  , reply
  ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Aeson                as A
import qualified Data.ByteString.Lazy      as BS
import qualified Data.HashMap.Strict       as HM
import qualified Data.Text                 as T
import qualified EuphApi.CloseableChan     as E
import qualified EuphApi.Types             as E
import qualified Network.WebSockets        as WS

{-
 - Some stuff
 -}

-- Some useful type aliases
type PacketID = T.Text
type Reply = Either Failure

-- | The ways in which getting a reply from the server can fail.
data Failure = FailDisconnect   -- ^ Disconnected from the server while waiting for the reply.
             | FailError T.Text -- ^ The server replied with an error.
             | FailParse        -- ^ Could not parse the server's reply correctly.

class ToJSONObject a where
  toJSONObject :: a -> Object

(.?=) :: (ToJSON v, KeyValue kv) => T.Text -> Maybe v -> [kv]
k .?= (Just v) = [k .= v]
k .?= Nothing  = []

packetOfType :: T.Text -> Value -> Object
packetOfType packetType packetData =
  HM.fromList [("type", A.String packetType), ("data", packetData)]

{-
 - Commands
 -}

data SendCommand = SendCommand
  { sendCommandContent :: T.Text
  , sendCommandParent  :: Maybe E.Snowflake
  } deriving (Show)

instance ToJSONObject SendCommand where
  toJSONObject (SendCommand{..}) =
    let obj = object $ ["content" .= sendCommandContent] ++ ("parent" .?= sendCommandParent)
    in  packetOfType "data" obj

-- send-reply
data SendReply = SendReply
  { sendReplyMessage :: E.Message
  } deriving (Show)

instance FromJSON SendReply where
  parseJSON v = SendReply <$> parseJSON v

{-
 - API functions
 -}

send :: SendChan -> T.Text -> IO (Reply E.Message)
send = undefined

reply :: SendChan -> PacketID -> T.Text -> IO (Reply E.Message)
reply = undefined

{-
 - Channels
 -}

type RecvChan = E.CloseableChan Recv
data Recv = RDisconnected
          | RPacket BS.ByteString
          | forall a . (FromJSON a) => RReply PacketID (MVar (Reply a))

type SendChan = E.CloseableChan Send
data Send = SDisconnect
          | forall p . (ToJSONObject p) => SNoReply p
          | forall p r . (ToJSONObject p, FromJSON r) => SReply p (MVar (Reply r))

type EventChan e = E.CloseableChan (Event e)
data Event e = EDisconnected
             | EStopped
             | EEuphEventPlaceholder
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
preparePacket :: (ToJSONObject p) => p -> SendState (BS.ByteString, PacketID)
preparePacket packet = do
  packetNr <- get
  put $ packetNr + 1
  let packetID = T.pack $ show packetNr
      obj      = HM.insert "id" (A.String packetID) $ toJSONObject packet
      bytestr  = encode $ Object obj
  return (bytestr, packetID)


sendThread :: SendChan -> RecvChan -> WS.Connection -> SendState ()
sendThread cSend cRecv con = do
  item <- liftIO $ E.readChan cSend
  case item of
    Nothing -> do
      return ()

    Just SDisconnect -> do
      liftIO $ WS.sendClose con ("Bye." :: T.Text)

    Just (SNoReply value) -> do
      (packet, _) <- preparePacket value
      liftIO $ WS.sendTextData con packet
      continue <- liftIO $ sendSafely packet
      if continue
        then sendThread cSend cRecv con
        else return ()

    Just (SReply value reply) -> do
      (packet, packetID) <- preparePacket value
      liftIO $ E.writeChan cRecv $ RReply packetID reply
      continue <- liftIO $ sendSafely packet
      if continue
        then sendThread cSend cRecv con
        else return ()
  where
    sendSafely packet = (WS.sendTextData con packet >> return True) `catch` handleException
    handleException (WS.CloseRequest _ _) = return False
    handleException WS.ConnectionClosed   = return False
    handleException _                     = return True

{-
 - RecvThread
 -}

-- TODO
