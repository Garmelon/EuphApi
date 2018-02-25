{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Monoid
import           System.Environment
import           System.IO

import qualified System.Log.Formatter      as LF
import qualified System.Log.Handler        as LH
import qualified System.Log.Handler.Simple as LH
import qualified System.Log.Logger         as L

import qualified EuphApi                   as E

type BotSpecific        = ()
type ConnectionSpecific = ()
type Bot     = E.Bot       BotSpecific ConnectionSpecific
type Config  = E.BotConfig BotSpecific ConnectionSpecific
type Command = E.Command   BotSpecific ConnectionSpecific


myCommands :: [Command]
myCommands =
  [ E.pingCommand "Pong!"
  , E.generalPingCommand "Pong!"
  , E.helpCommand (\n -> "Some specific placeholder help for " <> E.atMention n <> ".")
  , E.generalHelpCommand (const "I help test @Garmy's EuphApi.")
  , E.uptimeCommand
  , E.generalUptimeCommand -- most bots don't do this
  , E.killCommand "Bye!"
  , E.restartCommand "brb."
  ]

myBotHandler :: E.EventType -> Bot ()
myBotHandler (E.EuphEvent e) = E.autorunCommands myCommands e
myBotHandler _               = return ()

myBotConfig :: String -> Config
myBotConfig room = E.BotConfig
  { E.botAddress           = "euphoria.io"
  , E.botRoom              = room
  , E.botPassword          = Nothing
  , E.botNick              = "EuphApi test bot"
  , E.botHandler           = myBotHandler
  , E.botInfo              = ()
  , E.botNewConnectionInfo = return ()
  , E.botReconnectPolicy   = E.defaultReconnectPolicy
  }

main = do
  -- Set up logging with custom message style
  myHandler <- LH.verboseStreamHandler stdout L.INFO
  let myFormatter        = LF.simpleLogFormatter "<$time> [$loggername/$prio] $msg"
      myFormattedHandler = LH.setFormatter myHandler myFormatter
  L.updateGlobalLogger L.rootLoggerName (L.setHandlers [myFormattedHandler])
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.INFO)

  -- Use args to determine room and start the bot
  args <- getArgs
  case args of
    [room] -> E.runBot (return $ myBotConfig room)
    _      -> do
      name <- getProgName
      putStrLn "  USAGE:"
      putStr name
      putStrLn " <room>"
