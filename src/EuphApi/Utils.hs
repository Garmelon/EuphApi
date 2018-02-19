{-# LANGUAGE OverloadedStrings #-}

-- | Some useful functions for bots.

module EuphApi.Utils (
  -- * Nick manipulation
    mention
  , atMention
  , mentionReduce
  , similar
  -- * Commands
  , Command
  , CommandName
  , runCommands
  -- ** Creating commands
  , command
  , specificCommand
  , commandFromParser
  , withContent
  , withNick
  -- ** Useful parsers
  , mentionParser
  , atMentionParser
  , commandParser
  , specificCommandParser
  ) where

import           Control.Monad
import           Data.Char
import           Data.Function
import           Data.Void

import qualified Data.Text            as T
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P

import qualified EuphApi.Bot          as B
import qualified EuphApi.Types        as E

{-
 - Nick manipulation
 -}

-- | Convert a nick to an @-mentionable version.
-- Use this function when you want to @-mention somebody.
--
-- This removes spaces and some extra characters, while trying to stay close to
-- the original nick.
mention :: T.Text -> T.Text
mention = T.filter (\c -> not (isSpace c) && notElem c (".!?;&<'\"" :: String))

-- | Same as 'atMention', but prepends an `@` character.
atMention :: T.Text -> T.Text
atMention = T.cons '@' . mention

-- | Reduces a nick to a normal form such that all nicks that get @-mentioned
-- by the same @-mention are reduced to the same normal form.
--
-- Use this function when you want to compare two nicks.
mentionReduce :: T.Text -> T.Text
mentionReduce = T.map toLower . mention

-- | Compare two nicks using 'mentionReduce'.
similar :: T.Text -> T.Text -> Bool
similar = (==) `on` mentionReduce

{-
 - Commands
 -}

-- | A simple function that is to be called with the content of received messages.
--
-- If you just want to add a simple command, see 'command' and 'specificCommand'.
-- For more flexibility/more regex-like functionality, see 'commandFromParser'.
type Command b c = E.Message -> B.Bot b c ()

-- | Alias for the string after the @!@, for example: @\"help\"@ for the command: @!help@.
type CommandName = T.Text

-- | Runs a list of commands.
runCommands :: [Command b c] -> E.Message -> B.Bot b c ()
-- runCommands cs m = void $ sequence $ map ($m) cs
-- runCommands cs m = void . sequence $ cs <*> pure m
runCommands cs = void . sequence . sequence cs

withContent :: (T.Text -> a) -> E.Message -> a
withContent f = f . E.msgContent

withNick :: (T.Text -> a) -> B.Bot b c a
withNick f = (f . E.sessName) <$> B.getOwnView

-- | Creates a 'Command' from a parser and a bot action.
commandFromParser :: (Ord e)
                  => B.Bot b c (P.Parsec e T.Text a)
                  -> (a -> E.Message -> B.Bot b c ())
                  -> Command b c
commandFromParser p f m = do
  let content = E.msgContent m
  parser <- p
  forM_ (P.parseMaybe parser content) (`f` m)

type Parser = P.Parsec Void T.Text

command :: T.Text -> (E.Message -> B.Bot b c ()) -> Command b c
command c f =
  commandFromParser (return (commandParser c :: Parser () ))
                    (const f)

specificCommand :: T.Text -> (E.Message -> B.Bot b c ()) -> Command b c
specificCommand c f =
  commandFromParser (withNick (specificCommandParser c :: T.Text -> Parser () ))
                    (const f)

{-
 - Parsers
 -}

mentionParser :: (Ord e) => P.Parsec e T.Text T.Text
mentionParser = P.label "mention"
              $ P.takeWhile1P (Just "non-space character") (not . isSpace)

atMentionParser :: (Ord e) => P.Parsec e T.Text T.Text
atMentionParser = P.label "atMention" $ P.char '@' *> mentionParser

commandParser :: (Ord e) => T.Text -> P.Parsec e T.Text ()
commandParser c = P.label "command" $ do
  P.space
  void $ P.char '!' >> P.string c -- command
  P.space
  P.eof

specificCommandParser :: (Ord e) => T.Text -> T.Text -> P.Parsec e T.Text ()
specificCommandParser c nick = P.label "specific command" $ do
  P.space
  void $ P.char '!' >> P.string c -- command
  P.space1                        -- separator
  m <- atMentionParser            -- @mention
  guard $ m `similar` nick
  P.space
  P.eof
