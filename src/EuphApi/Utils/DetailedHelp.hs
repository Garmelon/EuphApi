{-# LANGUAGE OverloadedStrings #-}

-- | Help command that provides detailed help on certain topics.
--
-- @!help \@botname \<topic\>@

module EuphApi.Utils.DetailedHelp
  ( detailedHelpCommand
  , detailedHelpCommands
  , detailedHelpParser
  ) where

import           Control.Monad
import           Data.Void

import qualified Data.Text              as T
import qualified Text.Megaparsec        as P
import qualified Text.Megaparsec.Char   as P

import qualified EuphApi.Bot            as E
import qualified EuphApi.Types          as E
import qualified EuphApi.Utils.Commands as E

type Parser = P.Parsec Void T.Text

-- | Creates a detailed help command.
--
-- > import qualified EuphApi.Utils.Misc as E
-- >
-- > detailedHelpCommand
-- >   [ ("cheese", \n -> E.atMention n <> " likes cheese.")
-- >   , ("cabbage", \_ -> "Why are you asking about cabbage?")
-- >   ]
detailedHelpCommand :: [(E.CommandName, E.Nick -> T.Text)] -> E.Command b c
detailedHelpCommand = E.combineCommands . detailedHelpCommands

-- | Like 'detailedHelpCommand', but creates a list of commands instead.
detailedHelpCommands :: [(E.CommandName, E.Nick -> T.Text)] -> [E.Command b c]
detailedHelpCommands = map toCommand

toCommand :: (E.CommandName, E.Nick -> T.Text) -> E.Command b c
toCommand (name, f) = E.commandFromParser (E.withNick parser) $ const $ \msg -> do
  s <- E.sessName <$> E.getOwnView
  void $ E.replyTo msg (f s)
  where
    parser :: E.Nick -> Parser ()
    parser n = detailedHelpParser name n >> E.toEnd

-- | Parse a specific help command: @!command \@botname \<topic\>@
--
-- Use together with 'E.toEnd'.
detailedHelpParser :: (Ord e) => E.CommandName -> E.Nick -> P.Parsec e T.Text ()
detailedHelpParser name nick = do
  E.specificCommandParser "help" nick
  P.space1
  void $ P.string name
