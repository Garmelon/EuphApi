import qualified EuphApi.Bot        as E
import qualified EuphApi.Connection as E

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

main = E.runBot myBotConfig
