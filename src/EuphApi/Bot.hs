{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module lets you create bots, although it only contains the bare minimum necessary.
-- It defines the 'Bot' monad which takes care of a few things common to most bots.
--
-- The module exports some types from "EuphApi.Connection" for convenience.
-- Don't import both modules unless you know what you're doing.
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
-- For a simple example bot that just connects to <https://euphoria.io/room/test/ &test>
-- and follows all mandatory and most optional botrulez,
-- see <https://github.com/Garmelon/EuphApi/blob/master/test/bot_with_botrulez.hs bot_with_botrulez.hs>
-- in the tests folder of the repository.
-- This example bot also configures the logger to use a custom output format.
--
-- = Logging
--
-- This library uses the hslogger package for logging.
--
-- See <https://github.com/Garmelon/EuphApi/blob/master/test/bot_with_botrulez.hs bot_with_botrulez.hs>
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
  , getOwnView
  -- * Bot commands
  , stop
  , restart
  , send
  , reply
  , nick
  , getMessage
  , messageLog
  , messageLogAfter
  , who
  -- * Exceptions
  , BotException(..)
  -- * Misc
  , E.EventType(..)
  , E.Event(..)
  ) where

-- TODO: Add 'AuthenticationFailed' exception.

import           Control.Concurrent
import           Control.Exception
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

data ExitState = Stopping
               | Restarting

data BotState b c = BotState
  { bAddress           :: TVar String
  , bRoom              :: TVar String
  , bPassword          :: TVar (Maybe T.Text)
  , bNick              :: TVar T.Text
  , bHandler           :: E.EventType -> Bot b c ()
  , bBotInfo           :: b -- bot specific, user-defined info type
  , bNewConnectionInfo :: IO c
  , bReconnectPolicy   :: Integer -> Maybe Int
  , bStopping          :: TVar (Maybe ExitState)
  , bStartTime         :: UTCTime
  -- connection specific
  , bConnection        :: E.Connection
  , bConnectionInfo    :: c -- connection specific, user-defined info type
  , bConnectTime       :: UTCTime
  , bOwnView           :: TVar (Maybe E.SessionView)
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

-- | Returns the 'E.SessionView' of the current connection.
--
-- Might throw a 'NoOwnViewYet' exception.
getOwnView :: Bot b c E.SessionView
getOwnView = do
  var   <- asks bOwnView
  mView <- liftIO $ atomically $ readTVar var
  case mView of
    Just view -> return view
    Nothing   -> liftIO $ throwIO NoOwnViewYet

-- | A default reconnect policy that waits an exponentially increasing amount
-- of seconds between retries.
defaultReconnectPolicy :: Integer -> Maybe Int
defaultReconnectPolicy n = Just $ (2 ^ n) * 1000 * 1000 -- in microseconds

{-
 - Running a bot
 -}

runBotOnce :: BotConfig b c -> IO ExitState
runBotOnce BotConfig{..} = do
  bAddress        <- atomically $ newTVar botAddress
  bRoom           <- atomically $ newTVar botRoom
  bPassword       <- atomically $ newTVar $ T.pack <$> botPassword
  bNick           <- atomically $ newTVar $ T.pack botNick
  bStopping       <- atomically $ newTVar Nothing
  bOwnView        <- atomically $ newTVar Nothing
  noticeM $ "Connecting to &" ++ botRoom ++ " on " ++ show botAddress ++ "."
  bConnectionInfo <- botNewConnectionInfo
  bStartTime      <- getCurrentTime
  bConnection     <- E.startEuphConnection botAddress botRoom
  let bHandler           = botHandler
      bBotInfo           = botInfo
      bNewConnectionInfo = botNewConnectionInfo
      bReconnectPolicy   = botReconnectPolicy
      bConnectTime       = bStartTime
      state              = BotState{..}
  runReaderT (eventLoop 0) state

-- | Execute a bot in this thread.
--
-- Every time a bot is (re-)started, the 'IO' action passed to this function
-- will be executed to obtain a new 'BotConfig'.
-- This config is then used to run the bot.
runBot :: IO (BotConfig b c) -> IO ()
runBot ioConfig = do
  config <- ioConfig
  result <- runBotOnce config
  case result of
    Stopping   -> void $ noticeM "Bot has stopped."
    Restarting -> noticeM "Bot has restarted." >> runBot ioConfig

reconnect :: Integer -> Bot b c ExitState
reconnect retries = do
  state <- ask
  stopping <- liftIO $ atomically $ readTVar $ bStopping state
  case stopping of
    Just s  -> return s
    Nothing ->
      case bReconnectPolicy state retries of
        Nothing    -> liftIO $ throwIO OutOfRetries
        Just delay -> do
          liftIO $ infoM $ "Attempting reconnect in " ++ show (delay `div` 1000000)
                           ++ "s (" ++ show delay ++ "µs)."
          liftIO $ threadDelay delay
          address <- liftIO $ atomically $ readTVar $ bAddress state
          room    <- liftIO $ atomically $ readTVar $ bRoom    state
          now     <- liftIO getCurrentTime
          ownView <- liftIO $ atomically $ newTVar Nothing
          liftIO $ infoM $ "Reconnecting to &" ++ room ++ " on " ++ show address ++ "."
          conInfo <- liftIO $ bNewConnectionInfo state
          con     <- liftIO $ E.startEuphConnection address room
          let newState = state{ bConnection=con
                              , bConnectionInfo=conInfo
                              , bConnectTime=now
                              , bOwnView=ownView
                              }
          local (const newState) (eventLoop retries)
          -- lift $ runReaderT (eventLoop retries) newState

eventLoop :: Integer -> Bot b c ExitState
eventLoop retries = do
  con     <- getConnection
  handler <- asks bHandler
  event   <- liftIO $ E.getEvent con
  handler event
  case event of
    E.ConnectionFailed -> reconnect (retries + 1)
    E.Disconnected     -> reconnect 0
    E.EuphEvent e -> do
      handlePingStuff e
      handlePasswordStuff e
      handleOwnViewStuff e
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
  myNick    <- liftIO $ atomically $ readTVar myNickVar
  case maybeNick of
    Nothing -> fork $ nick myNick
    Just curNick ->
      if curNick == myNick
        then return ()
        else fork $ nick myNick
handleNickStuff _ = return ()

handlePasswordStuff :: E.Event -> Bot b c ()
handlePasswordStuff (E.BounceEvent _ (Just options))
  | "passcode" `elem` options = do
    myPasswordVar <- asks bPassword
    myPassword    <- liftIO $ atomically $ readTVar myPasswordVar
    con           <- getConnection
    case myPassword of
      Nothing -> liftIO $ throwIO PasswordNeeded
      Just p  -> fork $ liftIO $ E.auth con p
  | otherwise = liftIO $ throwIO NoValidAuthenticationMethods
handlePasswordStuff _ = return ()

handleOwnViewStuff :: E.Event -> Bot b c ()
handleOwnViewStuff (E.HelloEvent view _ _) = do
  var <- asks bOwnView
  liftIO $ debugM $ "Received new own view on HelloEvent: " ++ show view
  liftIO $ atomically $ writeTVar var (Just view)
handleOwnViewStuff (E.SnapshotEvent _ _ _ (Just curNick)) = do
  var <- asks bOwnView
  liftIO $ debugM "SnapshotEvent reported a nick. This should not happen in a bot."
  liftIO $ atomically $ changeOwnNick var curNick
handleOwnViewStuff _ = return ()

changeOwnNick :: TVar (Maybe E.SessionView) -> T.Text -> STM ()
changeOwnNick var newNick = do
  mView <- readTVar var
  case mView of
    Just view -> writeTVar var (Just view{E.sessName=newNick})
    Nothing   -> return ()

{-
 - Commands
 -}

stopWith :: ExitState -> Bot b c ()
stopWith s = do
  stopping <- asks bStopping
  con      <- asks bConnection
  liftIO $ do
    atomically $ writeTVar stopping (Just s)
    E.disconnect con

-- | Stop the bot.
stop :: Bot b c ()
stop = stopWith Stopping

-- | Restart the bot.
--
-- This will run the IO action passed to 'runBot' again to obtain a new 'BotConfig'.
-- Then, the new config is used to run the bot again.
restart :: Bot b c ()
restart = stopWith Restarting

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
  var    <- asks bOwnView
  con    <- asks bConnection
  liftIO $ do
    atomically $ writeTVar myNick newNick
    r@(from, to) <- E.nick con newNick
    infoM $ "Changed own nick from " ++ show from ++ " to " ++ show to ++ "."
    atomically $ changeOwnNick var to
    return r

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

{-
 - Exceptions
 -}

-- | Exceptions that the bot throws.
data BotException = NoOwnViewYet
                    -- ^ The bot has not received a SessionView for its current connection
                    -- from the server yet.
                  | PasswordNeeded
                    -- ^ The bot got bounced and needs a password to authenticate,
                    -- but none was provided in the config.
                  | NoValidAuthenticationMethods
                    -- ^ The bot got bounced, but the server didn't provide any
                    -- valid methods of authentication (password).
                    -- As long as the server is working properly, this exception should
                    -- not occur.
                  | OutOfRetries
                    -- ^ The bot's 'reconnectPolicy' has returned a @Nothing@ value,
                    -- meaning that the bot should not attempt to reconnect any further.

instance Show BotException where
  show NoOwnViewYet                 = "Bot hasn't received a SessionView of itself yet."
  show PasswordNeeded               = "Bot needs to authenticate, but has no password."
  show NoValidAuthenticationMethods = "Server gave no valid authentication methods."
  show OutOfRetries                 = "Bot has ran out of reconnect retries."

instance Exception BotException
