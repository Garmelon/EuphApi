{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module EuphApi.Bot (
  -- Creating a bot
    Bot
  , BotConfig(..)
  , runBot
  -- Utilities
  , fork
  , getBotInfo
  , getConnection
  , getConnectionInfo
  , defaultReconnectPolicy
  , respondToPing
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Concurrent.STM
import           Control.Monad.Trans.Reader
import qualified Data.Text                  as T
import qualified System.Log.Logger          as L

import qualified EuphApi.Connection         as E

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
  -- connection specific
  , bConnection        :: E.Connection
  , bConnectionInfo    :: c -- connection specific, user-defined info type
  }

type Bot b c = ReaderT (BotState b c) IO

data BotConfig b c = BotConfig
  { botAddress           :: String
  , botRoom              :: String
  , botPassword          :: Maybe String
  , botNick              :: String
  , botHandler           :: E.EventType -> Bot b c ()
  , botInfo              :: b
  , botNewConnectionInfo :: IO c
  , botReconnectPolicy   :: Integer -> Maybe Int
  }

fork :: Bot b c a -> Bot b c ()
fork bot = do
  state <- ask
  void $ liftIO $ forkIO $ void $ runReaderT bot state

getBotInfo :: Bot b c b
getBotInfo = asks bBotInfo

getConnection :: Bot b c E.Connection
getConnection = asks bConnection

getConnectionInfo :: Bot b c c
getConnectionInfo = asks bConnectionInfo

defaultReconnectPolicy :: Integer -> Maybe Int
defaultReconnectPolicy n = Just $ (2 ^ n) * 1000 * 1000 -- in microseconds

respondToPing :: E.EventType -> Bot b c ()
respondToPing (E.EuphEvent (E.PingEvent time _)) = do
  con <- getConnection
  liftIO $ E.pingReply con time -- TODO: Replace with bot version of the command
respondToPing _ = return ()

runBot :: BotConfig b c -> IO ()
runBot BotConfig{..} = do
  bAddress        <- atomically $ newTVar botAddress
  bRoom           <- atomically $ newTVar botRoom
  bPassword       <- atomically $ newTVar $ T.pack <$> botPassword
  bNick           <- atomically $ newTVar $ T.pack botNick
  bStopping       <- atomically $ newTVar False
  noticeM $ "Connecting to &" ++ botRoom ++ " on " ++ show botAddress ++ "."
  bConnection     <- E.startEuphConnection botAddress botRoom
  bConnectionInfo <- botNewConnectionInfo
  let bHandler           = botHandler
      bBotInfo           = botInfo
      bNewConnectionInfo = botNewConnectionInfo
      bReconnectPolicy   = botReconnectPolicy
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
          con     <- liftIO $ E.startEuphConnection address room
          liftIO $ infoM $ "Reconnecting to &" ++ room ++ " on " ++ show address ++ "."
          conInfo <- liftIO $ bNewConnectionInfo state
          let newState = state{bConnection=con, bConnectionInfo=conInfo}
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
      handlePasswordStuff e
      handleNickStuff e
      eventLoop retries

handleNickStuff :: E.Event -> Bot b c ()
handleNickStuff (E.SnapshotEvent _ _ _ maybeNick) = do
  myNickVar <- asks bNick
  myNick    <- liftIO $ atomically $ readTVar $ myNickVar
  con       <- getConnection
  case maybeNick of
    Nothing -> fork $ liftIO $ E.nick con myNick
    Just nick ->
      if nick == myNick
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
  | otherwise = return ()
handlePasswordStuff _ = return ()
