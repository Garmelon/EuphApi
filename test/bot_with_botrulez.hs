{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import           System.IO

import qualified Data.Text                 as T
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
  [ E.pingCommand
  , E.generalPingCommand
  , E.helpCommand "Some specific placeholder help"
  , E.generalHelpCommand "I help test @Garmy's EuphApi"
  , E.uptimeCommand
  , E.generalUptimeCommand
  , E.command "whatsmynick" (\msg -> do
      nick <- E.sessName <$> B.getOwnView
      let content = nick <> "\n" <> E.mention nick <> "\n" <> E.atMention nick <> "\n" <> E.mentionReduce nick
      void $ B.reply (E.msgID msg) content
    )
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
  myHandler <- LH.verboseStreamHandler stdout L.DEBUG
  let myFormatter        = LF.simpleLogFormatter "<$time> [$loggername/$prio] $msg"
      myFormattedHandler = LH.setFormatter myHandler myFormatter
  L.updateGlobalLogger L.rootLoggerName (L.setHandlers [myFormattedHandler])
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG)
  B.runBot myBotConfig
