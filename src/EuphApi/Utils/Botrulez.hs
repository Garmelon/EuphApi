{-# LANGUAGE OverloadedStrings #-}

module EuphApi.Utils.Botrulez
  ( pingCommand
  , generalPingCommand
  , helpCommand
  , generalHelpCommand
  , uptimeCommand
  , generalUptimeCommand
  , killCommand
  ) where

import           Control.Monad

import qualified Data.Text     as T

import qualified EuphApi.Bot   as B
import qualified EuphApi.Types as E
import qualified EuphApi.Utils as E

pingCommand :: E.Command b c
pingCommand = E.specificCommand "ping" $ \msg ->
  void $ B.reply (E.msgID msg) "Pong!"

generalPingCommand :: E.Command b c
generalPingCommand = E.command "ping" $ \msg ->
  void $ B.reply (E.msgID msg) "Pong!"

helpCommand :: T.Text -> E.Command b c
helpCommand helpText = E.specificCommand "help" $ \msg ->
  void $ B.reply (E.msgID msg) helpText

generalHelpCommand :: T.Text -> E.Command b c
generalHelpCommand helpText = E.command "help" $ \msg ->
  void $ B.reply (E.msgID msg) helpText

uptimeCommand :: E.Command b c
uptimeCommand = E.specificCommand "uptime" $ \msg ->
  void $ B.reply (E.msgID msg) "uptime placeholder"

generalUptimeCommand :: E.Command b c
generalUptimeCommand = E.command "uptime" $ \msg ->
  void $ B.reply (E.msgID msg) "uptime placeholder"

killCommand :: E.Command b c
killCommand = E.specificCommand "kill" $ \msg -> do
  void $ B.reply (E.msgID msg) "Bye!"
  B.stop
