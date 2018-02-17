import           System.IO

import qualified System.Log.Formatter      as LF
import qualified System.Log.Handler        as LH
import qualified System.Log.Handler.Simple as LH
import qualified System.Log.Logger         as L

import qualified EuphApi.Bot               as B

myBotConfig :: B.BotConfig () ()
myBotConfig = B.BotConfig
  { B.botAddress           = "euphoria.io"
  , B.botRoom              = "test"
  , B.botPassword          = Nothing
  , B.botNick              = "EuphApiTestBot"
  , B.botHandler           = const $ return ()
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
  B.runBot myBotConfig
