{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Monoid
import           System.IO

import qualified System.Log.Formatter      as LF
import qualified System.Log.Handler        as LH
import qualified System.Log.Handler.Simple as LH
import qualified System.Log.Logger         as L

import qualified EuphApi.Bot               as B
import qualified EuphApi.Connection        as E
import qualified EuphApi.Types             as E
import qualified EuphApi.Utils             as E
import qualified EuphApi.Utils.Botrulez    as E

myCommands :: [E.Command b c]
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

myBotHandler :: E.EventType -> B.Bot b c ()
myBotHandler (E.EuphEvent (E.SendEvent msg)) = E.runCommands myCommands msg
myBotHandler _                               = return ()

myBotConfig :: B.BotConfig () ()
myBotConfig = B.BotConfig
  { B.botAddress           = "euphoria.io"
  , B.botRoom              = "test"
  , B.botPassword          = Nothing
  , B.botNick              = "EuphApi test bot"
  , B.botHandler           = myBotHandler
  , B.botInfo              = ()
  , B.botNewConnectionInfo = return ()
  , B.botReconnectPolicy   = B.defaultReconnectPolicy
  }

main = do
  myHandler <- LH.verboseStreamHandler stdout L.INFO
  let myFormatter        = LF.simpleLogFormatter "<$time> [$loggername/$prio] $msg"
      myFormattedHandler = LH.setFormatter myHandler myFormatter
  L.updateGlobalLogger L.rootLoggerName (L.setHandlers [myFormattedHandler])
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.INFO)
  B.runBot (return myBotConfig)
