{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module lets you create bots, although it only contains the bare minimum necessary.
-- It defines the 'Bot' monad which takes care of a few things common to most bots.
--
-- The module is meant to be imported qualified, under a different name than all the
-- other EuphApi modules.
-- For example:
--
-- > import qualified EuphApi.Bot        as B
-- > import qualified EuphApi.Connection as E
-- > import qualified EuphApi.Types      as E
-- > import qualified EuphApi.Utils      as E
--
-- = The 'Bot' monad
--
-- This monad takes care of
--
--   - maintaining a 'E.Connection' to a specific room
--   - setting a nick (even after reconnecting)
--   - authenticating via password
--   - tracking starting and reconnect time
--   - keeping track of bot specific data and connection specific data
--
-- @Bot b c r@ stands for an action (within a bot) that returns @r@,
-- with bot specific data @b@ and connection specific data @c@.
--
-- Bot specific data is data that doesn't change while the bot runs.
-- It can either be used to keep track of bot configuration, or to keep
-- a few TVars/MVars for bot state (or even inter-bot communication).
--
-- Connection specific data is data that is specific to the current connection.
-- Examples for such data would be the listing of people or the 'E.SessionView'
-- describing the current session.
--
-- == Writing a bot
--
-- You will almost certainly want to write an own 'botHandler' so that your bot
-- can react to events from the server.
-- You don't need to respond to ping events or bounce events,
-- or make sure your nick is set correctly.
--
-- This means that a bot with @(const $ return ())@ as handler will still stay connected,
-- keep its nick and even authenticate if given a password.
--
-- __Important:__
-- Make sure to use 'fork' or 'forkIO' when using bot commands
-- or IO actions in your handler.
-- Otherwise the main bot thread will block while waiting for a response from the server.
--
-- If you want your bot to respond to commands, see "EuphApi.Utils.Commands".
--
-- If you want your bot to comply with the <https://github.com/jedevc/botrulez botrulez>,
-- see "EuphApi.Utils.Botulez".
--
-- For a simple example bot that just connects to <https://euphoria.io/room/test/ &test>,
-- see <https://github.com/Garmelon/EuphApi/blob/master/test/bot_simple_custom_logging.hs bot_simple_custom_logging.hs>
-- in the tests folder of the repository.
-- This example bot also configures the logger to use a custom output format.
--
-- = Logging
--
-- This library uses the hslogger package for logging.
--
-- See <https://github.com/Garmelon/EuphApi/blob/master/test/bot_simple_custom_logging.hs bot_simple_custom_logging.hs>
-- for an example of how to set the global format.

module EuphApi.Bot (
  -- * Creating a bot
    Bot
  , BotConfig(..)
  , runBot
  -- * Utilities
  , fork
  , defaultReconnectPolicy
  -- ** Context info
  , getBotInfo
  , getStartTime
  , getConnection
  , getConnectionInfo
  , getConnectTime
  -- * Bot commands
  , stop
  , send
  , reply
  , nick
  , getMessage
  , messageLog
  , messageLogAfter
  , who
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Concurrent.STM
import           Control.Monad.Trans.Reader
import qualified Data.Text                  as T
import           Data.Time
import qualified System.Log.Logger          as L

import qualified EuphApi.Connection         as E
import qualified EuphApi.Types              as E

-- logging functions
moduleName :: String
moduleName = "EuphApi.Bot"
debugM     :: String -> IO ()
debugM     = L.debugM moduleName
infoM      :: String -> IO ()
infoM      = L.infoM moduleName
noticeM    :: String -> IO ()
noticeM    = L.noticeM moduleName
--warningM   :: String -> IO ()
--warningM   = L.warningM moduleName
--errorM     :: String -> IO ()
--errorM     = L.errorM moduleName
--criticalM  :: String -> IO ()
--criticalM  = L.criticalM moduleName
--alertM     :: String -> IO ()
--alertM     = L.alertM moduleName
--emergencyM :: String -> IO ()
--emergencyM = L.emergencyM moduleName

data BotState b c = BotState
  { bAddress           :: TVar String
  , bRoom              :: TVar String
  , bPassword          :: TVar (Maybe T.Text)
  , bNick              :: TVar T.Text
  , bHandler           :: E.EventType -> Bot b c ()
  , bBotInfo           :: b -- bot specific, user-defined info type
  , bNewConnectionInfo :: IO c
  , bReconnectPolicy   :: Integer -> Maybe Int
  , bStopping          :: TVar Bool
  , bStartTime         :: UTCTime
  -- connection specific
  , bConnection        :: E.Connection
  , bConnectionInfo    :: c -- connection specific, user-defined info type
  , bConnectTime       :: UTCTime
  }

-- | The monad that bots are written in.
--
-- @Bot b c r@ stands for an action (within a bot) that returns @r@,
-- with bot specific data @b@ and connection specific data @c@.
--
-- Don't worry about what @BotState@ is.
-- I might one day add a newtype for Bot.
type Bot b c = ReaderT (BotState b c) IO

-- | Configuration for a bot.
--
-- Create one of these and use them with 'runBot' to run your bot.
data BotConfig b c = BotConfig
  { botAddress           :: String
    -- ^ Address of the server you want to connect to.
    --
    -- Unless you have a heim clone lying around somewhere,
    -- use @\"euphoria.io\"@.
  , botRoom              :: String
    -- ^ Name of the room to connect to.
  , botPassword          :: Maybe String
    -- ^ Password of the room, if any.
  , botNick              :: String
    -- ^ Nick that the bot should use.
  , botHandler           :: E.EventType -> Bot b c ()
    -- ^ This handler gets called whenever the bot receives an event from the server.
    -- Your bot logic goes here.
    --
    -- Make sure you use 'fork' when calling bot commands.
  , botInfo              :: b
    -- ^ Bot specific data.
  , botNewConnectionInfo :: IO c
    -- ^ Create new instance of $c$ to use with a new connection.
  , botReconnectPolicy   :: Integer -> Maybe Int
    -- ^ A function that determines how long, if at all, the bot should wait
    -- before attempting to reconnect after the n-th try.
    -- Returns the time in µs (to get seconds, divide by 1000000).
    --
    -- > reconnectPolicy n (Maybe waitTime)
  }

{-
 - Helpful functions
 -}

-- | Run this bot action in a separate thread.
fork :: Bot b c a -> Bot b c ()
fork bot = do
  state <- ask
  void $ liftIO $ forkIO $ void $ runReaderT bot state

-- | Returns the @Bot b c a@'s bot specific info @b@.
getBotInfo :: Bot b c b
getBotInfo = asks bBotInfo

-- | Returns the time when the bot was first started.
getStartTime :: Bot b c UTCTime
getStartTime = asks bStartTime

-- | Returns the time when the bot last connected to the room.
getConnectTime :: Bot b c UTCTime
getConnectTime = asks bConnectTime

-- | Returns the 'E.Connection' the bot currently uses.
--
-- __Warning:__ Only use this if you know what you're doing.
getConnection :: Bot b c E.Connection
getConnection = asks bConnection

-- | Returns the @Bot b c a@'s connection specific info @c@.
getConnectionInfo :: Bot b c c
getConnectionInfo = asks bConnectionInfo

-- | A default reconnect policy that waits an exponentially increasing amount
-- of seconds between retries.
defaultReconnectPolicy :: Integer -> Maybe Int
defaultReconnectPolicy n = Just $ (2 ^ n) * 1000 * 1000 -- in microseconds

{-
 - Running a bot
 -}

-- | Execute a bot in this thread.
runBot :: BotConfig b c -> IO ()
runBot BotConfig{..} = do
  bAddress        <- atomically $ newTVar botAddress
  bRoom           <- atomically $ newTVar botRoom
  bPassword       <- atomically $ newTVar $ T.pack <$> botPassword
  bNick           <- atomically $ newTVar $ T.pack botNick
  bStopping       <- atomically $ newTVar False
  noticeM $ "Connecting to &" ++ botRoom ++ " on " ++ show botAddress ++ "."
  bStartTime      <- getCurrentTime
  bConnection     <- E.startEuphConnection botAddress botRoom
  bConnectionInfo <- botNewConnectionInfo
  let bHandler           = botHandler
      bBotInfo           = botInfo
      bNewConnectionInfo = botNewConnectionInfo
      bReconnectPolicy   = botReconnectPolicy
      bConnectTime       = bStartTime
      state              = BotState{..}
  runReaderT (eventLoop 0) state

reconnect :: Integer -> Bot b c ()
reconnect retries = do
  state <- ask
  stopping <- liftIO $ atomically $ readTVar $ bStopping state
  if stopping
    then return ()
    else
      case (bReconnectPolicy state $ retries) of
        Nothing    -> return ()
        Just delay -> do
          liftIO $ infoM $ "Attempting reconnect in " ++ show (delay `div` 1000000)
                           ++ "s (" ++ show delay ++ "µs)."
          liftIO $ threadDelay delay
          address <- liftIO $ atomically $ readTVar $ bAddress state
          room    <- liftIO $ atomically $ readTVar $ bRoom    state
          now     <- liftIO $ getCurrentTime
          con     <- liftIO $ E.startEuphConnection address room
          liftIO $ infoM $ "Reconnecting to &" ++ room ++ " on " ++ show address ++ "."
          conInfo <- liftIO $ bNewConnectionInfo state
          let newState = state{bConnection=con, bConnectionInfo=conInfo, bConnectTime=now}
          local (const newState) (eventLoop retries)
          -- lift $ runReaderT (eventLoop retries) newState

eventLoop :: Integer -> Bot b c ()
eventLoop retries = do
  con     <- getConnection
  handler <- asks bHandler
  event   <- liftIO $ E.getEvent con
  liftIO $ debugM $ "Received event: " ++ show event
  handler event
  case event of
    E.ConnectionFailed -> reconnect (retries + 1)
    E.Disconnected     -> reconnect 0
    E.EuphEvent e -> do
      handlePingStuff e
      handlePasswordStuff e
      handleNickStuff e
      eventLoop retries

handlePingStuff :: E.Event -> Bot b c ()
handlePingStuff (E.PingEvent time _) = do
  con <- getConnection
  liftIO $ E.pingReply con time
handlePingStuff _ = return ()

handleNickStuff :: E.Event -> Bot b c ()
handleNickStuff (E.SnapshotEvent _ _ _ maybeNick) = do
  myNickVar <- asks bNick
  myNick    <- liftIO $ atomically $ readTVar $ myNickVar
  con       <- getConnection
  case maybeNick of
    Nothing -> fork $ liftIO $ E.nick con myNick
    Just curNick ->
      if curNick == myNick
        then return ()
        else fork $ liftIO $ E.nick con myNick
handleNickStuff _ = return ()

handlePasswordStuff :: E.Event -> Bot b c ()
handlePasswordStuff (E.BounceEvent _ (Just options))
  | "passcode" `elem` options = do
    myPasswordVar <- asks bPassword
    myPassword    <- liftIO $ atomically $ readTVar $ myPasswordVar
    con           <- getConnection
    case myPassword of
      Nothing -> fork $ liftIO $ E.disconnect con -- TODO: Do something here
      Just p  -> fork $ liftIO $ E.auth con p
  | otherwise = return () -- TODO: And also here
handlePasswordStuff _ = return ()

{-
 - Commands
 -}

-- | Stop the bot.
stop :: Bot b c ()
stop = do
  stopping <- asks bStopping
  con <- asks bConnection
  liftIO $ do
    atomically $ writeTVar stopping False
    E.disconnect con

-- | Send a new message.
send :: T.Text -> Bot b c E.Message
send content = do
  con <- asks bConnection
  liftIO $ E.send con Nothing content

-- | Reply to a message.
reply :: E.Snowflake -> T.Text -> Bot b c E.Message
reply parentID content = do
  con <- asks bConnection
  liftIO $ E.send con (Just parentID) content

-- | Change the bot's nick.
nick :: T.Text -> Bot b c (T.Text, T.Text)
nick newNick = do
  myNick <- asks bNick
  con <- asks bConnection
  liftIO $ do
    atomically $ writeTVar myNick newNick
    E.nick con newNick

-- | Request an untruncated message.
getMessage :: E.Snowflake -> Bot b c E.Message
getMessage messageID = do
  con <- asks bConnection
  liftIO $ E.getMessage con messageID

-- | Request the n most recent messages (similar to the 'E.SnapshotEvent').
messageLog :: Integer -> Bot b c ([E.Message], Maybe E.Snowflake)
messageLog n = do
  con <- asks bConnection
  liftIO $ E.messageLog con n Nothing

-- | Request the n messages preceding a certain message.
messageLogAfter :: E.Snowflake -> Integer -> Bot b c ([E.Message], Maybe E.Snowflake)
messageLogAfter messageID n = do
  con <- asks bConnection
  liftIO $ E.messageLog con n (Just messageID)

-- | Request a listing of all currently connected sessions.
who :: Bot b c [E.SessionView]
who = do
  con <- asks bConnection
  liftIO $ E.who con
