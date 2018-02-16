import           System.IO

import qualified System.Log.Formatter      as LF
import qualified System.Log.Handler        as LH
import qualified System.Log.Handler.Simple as LH
import qualified System.Log.Logger         as L

import qualified EuphApi.Bot               as E
import qualified EuphApi.Connection        as E

myBotConfig :: E.BotConfig () ()
myBotConfig = E.BotConfig
  { E.botAddress           = "euphoria.io"
  , E.botRoom              = "test"
  , E.botPassword          = Nothing
  , E.botNick              = "EuphApiTestBot"
  , E.botHandler           = E.respondToPing
  , E.botInfo              = ()
  , E.botNewConnectionInfo = return ()
  , E.botReconnectPolicy   = E.defaultReconnectPolicy
  }

main = do
  myHandler <- LH.verboseStreamHandler stderr L.INFO
  let myFormatter        = LF.simpleLogFormatter "<$time> [$loggername/$prio] $msg"
      myFormattedHandler = LH.setFormatter myHandler myFormatter
  L.updateGlobalLogger L.rootLoggerName (L.setHandlers [myFormattedHandler])
  L.updateGlobalLogger L.rootLoggerName (L.setLevel L.INFO)
  E.runBot myBotConfig
