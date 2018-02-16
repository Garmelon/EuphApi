{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Monoid
import           System.Environment

import qualified Data.Text          as T

import qualified EuphApi            as E
import qualified EuphApi.Connection as E


runBot :: String -> IO ()
runBot room = do
  con <- E.startEuphConnection "euphoria.io" room
  handleEvents room con

handleEvents :: String -> E.Connection -> IO ()
handleEvents room con = do
  event <- E.getEvent con
  case event of
    E.Disconnected     -> runBot room
    E.ConnectionFailed -> putStrLn $ "Could not connect to &" ++ room ++ "."
    E.EuphEvent e      -> handleEuphEvent con e >> handleEvents room con

handleEuphEvent :: E.Connection -> E.Event -> IO ()
handleEuphEvent con (E.PingEvent time _) = do
  E.pingReply con time
  putStrLn "Pong!"
handleEuphEvent con (E.BounceEvent _ _) = do
  E.disconnect con
  putStrLn "Room is private. And I don't have a password."
handleEuphEvent con (E.HelloEvent _ _ _) = do
  void $ E.nick con "EuphApi test bot"
  putStrLn "Set nick"
handleEuphEvent con (E.JoinEvent sess) = do
  let msg = "Hello, " <> E.sessName sess <> "!"
  void $ E.send con Nothing msg
handleEuphEvent con (E.PartEvent sess) = do
  let msg = "Bye, " <> E.sessName sess <> "!"
  void $ E.send con Nothing msg
handleEuphEvent con (E.NickEvent from to) = do
  let msg = T.pack $ "From " ++ show from ++ " to " ++ show to ++ "..."
  void $ E.send con Nothing msg
handleEuphEvent con (E.SendEvent message)
  | E.msgContent message == "haskell"  = void $ E.send con (Just $ E.msgID message) "awesome!"
  | E.msgContent message == "euphoria" = void $ E.send con (Just $ E.msgID message) ":euphoria!:"
  | otherwise                          = return ()
handleEuphEvent _ _ = return ()

main = do
  args <- getArgs
  case args of
    [room] -> putStrLn ("Connecting to " ++ room) >> runBot room
    _      -> putStrLn "wrong arguments"
