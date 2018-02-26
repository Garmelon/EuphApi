-- | A bot library for <https://euphoria.io/ euphoria>.
--
-- If you're looking to write a bot, see the "EuphApi.Bot" module for more detail.
-- There is also an
-- <https://github.com/Garmelon/EuphApi/blob/master/test/bot_with_botrulez.hs example bot>
-- in the /test/ directory.
--
-- If you want api bindings for euphoria without any of the bot stuff, see the
-- "Euphoria.Connection" module.
--
-- = Overview of the modules
--
-- ["EuphApi"] Reexports a few modules useful for creating bots.
-- ["EuphApi.Bot"] Bot structure and info on how to create bots.
-- ["EuphApi.Connection"] \"Raw\" API bindings.
-- ["EuphApi.Types"] Some common types for the API.
-- ["EuphApi.Utils"] Reexports a few util modules useful for creating bots.
-- ["EuphApi.Utils.Botrulez"] <https://github.com/jedevc/botrulez botrulez> commands.
-- ["EuphApi.Utils.Commands"] General and specific bot commands.
-- ["EuphApi.Utils.DetailedHelp"] Help command for more detailed help about special topics:
--                                @!help \@botname \<topic\>@
-- ["EuphApi.Utils.Listing"] Track which clients are connected to the room.
-- ["EuphApi.Utils.Misc"] Functions for dealing with nicks and time formats.

module EuphApi
  ( module EuphApi.Bot
  , module EuphApi.Types
  , module EuphApi.Utils
  ) where

import           EuphApi.Bot
import           EuphApi.Types
import           EuphApi.Utils
