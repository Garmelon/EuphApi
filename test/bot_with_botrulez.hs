{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Monoid
import           System.IO

import qualified System.Log.Formatter      as LF
import qualified System.Log.Handler        as LH
import qualified System.Log.Handler.Simple as LH
import qualified System.Log.Logger         as L

import qualified EuphApi                   as E

type Bot = E.Bot () ()
type Command = E.Command () ()

myCommands :: [Command]
myCommands =
  [ E.pingCommand "Pong!"
  , E.generalPingCommand "Pong!"
  , E.helpCommand "Some specific placeholder help"
  , E.generalHelpCommand "I help test @Garmy's EuphApi"
  , E.uptimeCommand
  , E.generalUptimeCommand -- most bots don't do this
  , E.killCommand "Bye!"
  , E.restartCommand "brb"
  ]

myBotHandler :: E.EventType -> Bot ()
myBotHandler (E.EuphEvent (E.SendEvent msg)) = E.runCommands myCommands msg
myBotHandler _                               = return ()

myBotConfig :: E.BotConfig () ()
myBotConfig = E.BotConfig
  { E.botAddress           = "euphoria.io"
  , E.botRoom              = "test"
  , E.botPassword          = Nothing
  , E.botNick              = "EuphApi test bot"
  , E.botHandler           = myBotHandler
  , E.botInfo              = ()
  , E.botNewConnectionInfo = return ()
  , E.botReconnectPolicy   = E.defaultReconnectPolicy
  }

main = do
  myHandler <- LH.verboseStreamHandler stdout L.INFO
  let myFormatter        = LF.simpleLogFormatter "<$time> [$loggername/$prio] $msg"
      myFormattedHandler = LH.setFormatter myHandler myFormatter
  L.updateGlobalLogger L.rootLoggerName (L.setHandlers [myFormattedHandler])
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.INFO)
  E.runBot (return myBotConfig)
