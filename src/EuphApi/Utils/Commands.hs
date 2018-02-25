{-# LANGUAGE OverloadedStrings #-}

-- | This module helps with letting bots respond to commands.
-- It supports general and specific commands.
--
-- If you want your bot to react to things other than commands like @!command@,
-- have a look at the 'commandFromParser' function and the parsers below.
-- It may often be easier to write a simple parser than to use regular expressions.

module EuphApi.Utils.Commands
  ( Command
  , CommandName
  , runCommands
  , autorunCommands
  -- * Creating commands
  , command
  , specificCommand
  , commandFromParser
  -- * Useful parsers
  , mentionParser
  , atMentionParser
  , commandParser
  , specificCommandParser
  ) where

import           Control.Monad
import           Data.Char
import           Data.Void

import qualified Data.Text            as T
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P

import qualified EuphApi.Bot          as E
import qualified EuphApi.Types        as E
import qualified EuphApi.Utils.Misc   as E

-- | A simple function that is to be called with the content of received messages.
--
-- If you just want to add a simple command, see 'command' and 'specificCommand'.
-- For more flexibility/more regex-like functionality, see 'commandFromParser'.
type Command b c = E.Message -> E.Bot b c ()

-- | Alias for the string after the @!@, for example: @\"help\"@ for the command: @!help@.
type CommandName = T.Text

-- | Runs a list of commands.
runCommands :: [Command b c] -> E.Message -> E.Bot b c ()
-- runCommands cs m = mapM_ E.fork $ map ($m) cs
-- runCommands cs m = mapM_ E.fork $ cs <*> pure m
-- runCommands cs = mapM_ E.fork . sequence cs
runCommands cs m = mapM_ (E.fork . ($m)) cs

-- | Atomatically run commands as necessary, according to the 'E.Event' given.
autorunCommands :: [Command b c] -> E.Event -> E.Bot b c ()
autorunCommands cs (E.SendEvent msg) = runCommands cs msg
autorunCommands _ _                  = return ()

withNick :: (T.Text -> a) -> E.Bot b c a
withNick f = (f . E.sessName) <$> E.getOwnView

-- | Creates a 'Command' from a parser and a bot action.
--
-- > commandFromParser parser action
--
-- The parser is enclosed in a 'E.Bot' so that it may access the bot's current state,
-- for example the bot's current nick.
commandFromParser :: (Ord e)
                  => E.Bot b c (P.Parsec e T.Text a)
                  -> (a -> E.Message -> E.Bot b c ())
                  -> Command b c
commandFromParser p f m = do
  let content = E.msgContent m
  parser <- p
  forM_ (P.parseMaybe parser content) (`f` m)

type Parser = P.Parsec Void T.Text

-- | Creates a general command: @!command@
--
-- If you want to parse arguments too, use 'commandFromParser'
-- and write your own parser using 'commandParser'.
command :: T.Text -> (E.Message -> E.Bot b c ()) -> Command b c
command c f =
  commandFromParser (return (commandParser c :: Parser () ))
                    (const f)

-- | Creates a specific command: @!command \@botname@
--
-- If you want to parse arguments too, use 'commandFromParser'
-- and write your own parser using 'specificCommandParser'.
specificCommand :: T.Text -> (E.Message -> E.Bot b c ()) -> Command b c
specificCommand c f =
  commandFromParser (withNick (specificCommandParser c :: T.Text -> Parser () ))
                    (const f)

{-
 - Parsers
 -}

-- | Parse a mention (without the @\@@).
--
-- This parser basically parses all non-space characters until the next space
-- or end of input:
--
-- @'P.takeWhile1P' (Just \"non-space character\") (not . 'isSpace')@
mentionParser :: (Ord e) => P.Parsec e T.Text T.Text
mentionParser = P.label "mention"
              $ P.takeWhile1P (Just "non-space character") (not . isSpace)

-- | Similar to 'mentionParser', but includes the @\@@.
atMentionParser :: (Ord e) => P.Parsec e T.Text T.Text
atMentionParser = P.label "atMention" $ P.char '@' *> mentionParser

-- | Parse a general command: @!command@
commandParser :: (Ord e) => T.Text -> P.Parsec e T.Text ()
commandParser c = P.label "command" $ do
  P.space
  void $ P.char '!' >> P.string c -- command
  P.space
  P.eof

-- | Parse a specific command: @!command \@botname@
specificCommandParser :: (Ord e) => T.Text -> T.Text -> P.Parsec e T.Text ()
specificCommandParser c nick = P.label "specific command" $ do
  P.space
  void $ P.char '!' >> P.string c -- command
  P.space1                        -- separator
  m <- atMentionParser            -- @mention
  guard $ m `E.similar` nick
  P.space
  P.eof
