-- | Some useful functions for bots.

module EuphApi.Utils (
  -- * Nick manipulation
    mention
  , atMention
  , mentionReduce
  -- * Commands
  , Command
  , runCommands
  , runCommandsFromMessage
  -- ** Creating commands
  , command
  , specificCommand
  , commandFromParser
  ) where

import           Control.Monad
import           Data.Char

import qualified Data.Text       as T
import qualified Text.Megaparsec as P

import qualified EuphApi.Bot     as B
import qualified EuphApi.Types   as E

{-
 - Nick manipulation
 -}

-- | Convert a nick to an @-mentionable version.
-- Use this function when you want to @-mention somebody.
--
-- This removes spaces and some extra characters, while trying to stay close to
-- the original nick.
mention :: T.Text -> T.Text
mention = T.filter (\c -> not (isSpace c) && notElem c ".!?;&<'\"")

-- | Same as 'atMention', but prepends an `@` character.
atMention :: T.Text -> T.Text
atMention = T.cons '@' . mention

-- | Reduces a nick to a normal form such that all nicks that get @-mentioned
-- by the same @-mention are reduced to the same normal form.
--
-- Use this function when you want to compare two nicks.
mentionReduce :: T.Text -> T.Text
mentionReduce = T.map toLower . mention

{-
 - Commands
 -}

-- | A simple function that is to be called with the content of received messages.
--
-- If you just want to add a simple command, see 'command' and 'specificCommand'.
-- For more flexibility/more regex-like functionality, see 'commandFromParser'.
type Command b c = T.Text -> B.Bot b c ()

-- | Runs a list of commands.
runCommands :: [Command b c] -> T.Text -> B.Bot b c ()
-- runCommands cs t = void $ sequence $ map ($t) cs
-- runCommands cs t = void . sequence $ cs <*> pure t
runCommands cs = void . sequence . sequence cs

-- | Runs a list of commands, using the content of a 'E.Message'.
runCommandsFromMessage :: [Command b c] -> E.Message -> B.Bot b c ()
runCommandsFromMessage cs = runCommands cs . E.msgContent

-- | Creates a 'Command' from a parser and a bot action.
commandFromParser :: (Ord e) => P.Parsec e T.Text a -> (a -> B.Bot b c ()) -> Command b c
commandFromParser p f t = maybe (return ()) f $ P.parseMaybe p t

command :: T.Text -> B.Bot b c () -> Command b c
command = undefined

specificCommand :: T.Text -> B.Bot b c () -> Command b c
specificCommand = undefined
