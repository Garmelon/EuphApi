{-# LANGUAGE OverloadedStrings #-}

-- | This module contains most of the commands specified in the
-- <https://github.com/jedevc/botrulez botrulez>.

module EuphApi.Utils.Botrulez
  ( pingCommand
  , generalPingCommand
  , helpCommand
  , generalHelpCommand
  , uptimeCommand
  , generalUptimeCommand
  , killCommand
  , killCommandSilent
  , restartCommand
  , restartCommandSilent
  ) where

import           Control.Monad
import           Control.Monad.IO.Class

import qualified Data.Text              as T
import           Data.Time

import qualified EuphApi.Bot            as E
import qualified EuphApi.Types          as E
import qualified EuphApi.Utils.Commands as E
import qualified EuphApi.Utils.Misc     as E

-- | Specific ping command: @!ping \@botname@
--
-- Bots should reply @\"Pong!\"@ or a similar short message.
--
-- Bots __should implement__ this command.
pingCommand :: T.Text -> E.Command b c
pingCommand pingText = E.specificCommand "ping" $ \msg ->
  void $ E.reply (E.msgID msg) pingText

-- | General version of 'pingCommand': @!ping@
--
-- Bots __should implement__ this command.
generalPingCommand :: T.Text -> E.Command b c
generalPingCommand pingText = E.command "ping" $ \msg ->
  void $ E.reply (E.msgID msg) pingText

-- | Specific help command: @!help \@botname@
--
-- Bots should reply with a detailed help message.
--
-- Bots __should implement__ this command.
helpCommand :: T.Text -> E.Command b c
helpCommand helpText = E.specificCommand "help" $ \msg ->
  void $ E.reply (E.msgID msg) helpText

-- | General version of 'helpCommand': @!help@
--
-- Bots should reply with a short description of their function.
--
-- Bots __may implement__ this command.
generalHelpCommand :: T.Text -> E.Command b c
generalHelpCommand helpText = E.command "help" $ \msg ->
  void $ E.reply (E.msgID msg) helpText

uptime :: E.Message -> E.Bot b c ()
uptime msg = do
  startTime <- E.getStartTime
  curTime <- liftIO getCurrentTime
  void $ E.reply (E.msgID msg) (T.pack $ E.printUptime startTime curTime)

-- | Specific uptime command: @!uptime \@botname@
--
-- Bots should reply with the time since they were started.
-- For the format this command uses, see 'E.printUptime'.
--
-- Bots __should implement__ this command.
uptimeCommand :: E.Command b c
uptimeCommand = E.specificCommand "uptime" uptime

-- | General version of 'uptimeCommand': @!uptime@
--
-- Bots __may implement__ this command.
generalUptimeCommand :: E.Command b c
generalUptimeCommand = E.command "uptime" uptime

-- | Specific kill command: @!kill \@botname@
--
-- When killed, bots should disconnect and not reconnect.
--
-- Bots __may implement__ this command.
killCommand :: T.Text -> E.Command b c
killCommand t = E.specificCommand "kill" $ \msg -> do
  void $ E.reply (E.msgID msg) t
  E.stop

-- | Version of 'killCommand' where the bot does not reply to the message which kills it.
killCommandSilent :: E.Command b c
killCommandSilent = E.specificCommand "kill" $ const E.stop

-- | Specific restart command: @!restart \@botname@
--
-- When restarted, the bot receiving the command should be !killed and a
-- new instance of the same bot should be started.
--
-- Bots __may implement__ this command.
restartCommand :: T.Text -> E.Command b c
restartCommand t = E.specificCommand "restart" $ \msg -> do
  void $ E.reply (E.msgID msg) t
  E.restart

-- | Version of 'restartCommand' where the bot does not reply to the message
-- which restarts it.
restartCommandSilent :: E.Command b c
restartCommandSilent = E.specificCommand "restart" $ const E.restart
