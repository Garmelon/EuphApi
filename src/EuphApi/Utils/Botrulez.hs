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
import           Control.Monad.IO.Class

import qualified Data.Text              as T
import           Data.Time

import qualified EuphApi.Bot            as B
import qualified EuphApi.Types          as E
import qualified EuphApi.Utils          as E

pingCommand :: T.Text -> E.Command b c
pingCommand pingText = E.specificCommand "ping" $ \msg ->
  void $ B.reply (E.msgID msg) pingText

generalPingCommand :: T.Text -> E.Command b c
generalPingCommand pingText = E.command "ping" $ \msg ->
  void $ B.reply (E.msgID msg) pingText

helpCommand :: T.Text -> E.Command b c
helpCommand helpText = E.specificCommand "help" $ \msg ->
  void $ B.reply (E.msgID msg) helpText

generalHelpCommand :: T.Text -> E.Command b c
generalHelpCommand helpText = E.command "help" $ \msg ->
  void $ B.reply (E.msgID msg) helpText

uptime :: E.Message -> B.Bot b c ()
uptime msg = do
  startTime <- B.getStartTime
  curTime <- liftIO getCurrentTime
  void $ B.reply (E.msgID msg) (T.pack $ E.printUptime startTime curTime)

uptimeCommand :: E.Command b c
uptimeCommand = E.specificCommand "uptime" uptime

generalUptimeCommand :: E.Command b c
generalUptimeCommand = E.command "uptime" uptime

killCommand :: E.Command b c
killCommand = E.specificCommand "kill" $ \msg -> do
  void $ B.reply (E.msgID msg) "Bye!"
  B.stop
