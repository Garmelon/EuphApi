{-# LANGUAGE ExistentialQuantification #-}

-- | Setup consisting of a few threads to send and receive packets to and from 
-- the euphoria api using a websocket connection.

module EuphApi.Threads (
  -- * Events and replies
    Failure(..)
  -- * Functions for using the api
  , send
  , reply
  ) where

import           Control.Concurrent
import           Control.Exception
import           Data.Aeson
import           Data.Text
import qualified EuphApi.Types      as E
import qualified Network.WebSockets as WS

-- Some useful type aliases
type PacketID = Text

{-
 - Events and replies
 -}

-- | The ways in which getting a reply from the server can fail.
data Failure = FailDisconnect -- ^ Disconnected from the server while waiting for the reply.
             | FailParse      -- ^ Could not parse the server's reply correctly.

-- send-reply
data SendReply = SendReply
  { sendReplyMessage :: E.Message
  } deriving (Show)

instance FromJSON SendReply where
  parseJSON v = SendReply <$> parseJSON v

{-
 - API functions
 -}

send :: SendChan -> Text -> IO (Either Failure E.Message)
send = undefined

reply :: SendChan -> PacketID -> Text -> IO (Either Failure E.Message)
reply = undefined






























{-
data Packet = Packet
  { packetID :: Maybe PacketID
  , packetType :: Text
  , packetContent :: Content
  , packetThrottled :: Maybe Text
  }
-}

type SendChan = Chan Send
-- Contents of sendChan
data Send = SPacket Text --Value -- packet type, content
          | SDisconnect

type RecvChan = Chan Recv
-- Contents of recvChan
data Recv = RConnectionClosed -- Ws connection closed
-- | RPacket ByteString -- Packet received from the ws connection
-- | forall c . (FromJSON c) => RReply PacketID (MVar (Response c)) -- Request for a reply with a certain ID

{-
sendPacket :: Connection -> Packet -> IO ()
sendPacket = undefined

recvPacket :: Connection -> IO Packet
recvPacket = undefined

sendThread :: SendChan -> RecvChan -> Connection -> IO ()
sendThread s r c = do
  return ()

type EventChan = Chan Event
-- Contents of eventChan
data Event = EPlaceholder

fetchMessage :: RecvChan -> Connection -> IO ()
fetchMessage recv con = do
  message <- receiveData con
  writeChan recv (RPacket message)
  fetchMessage recv con

fetchThread :: RecvChan -> Connection -> IO ()
fetchThread recv con = fetchMessage recv con `catch` handleException
  where
    handleException (CloseRequest _ _) = writeChan recv RConnectionClosed
    handleException ConnectionClosed   = writeChan recv RConnectionClosed
    handleException _                  = fetchThread recv con

sendMessage :: SendChan -> RecvChan -> Connection -> IO ()
sendMessage send recv con = do
  message <- readChan send
  return ()

sendThread :: SendChan -> RecvChan -> Connection -> IO ()
sendThread = undefined
-}
