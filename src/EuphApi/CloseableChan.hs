{-# LANGUAGE RecordWildCards #-}

module EuphApi.CloseableChan
  ( CloseableChan
  -- * IO versions
  , newCloseableChan
  , writeChan
  , readChan
  , closeChan
  , emptyChan
  -- * STM versions
  , newCloseableChanSTM
  , writeChanSTM
  , readChanSTM
  , closeChanSTM
  , emptyChanSTM
  ) where

import           Control.Concurrent.STM
import           Control.Monad

data CloseableChan a = CloseableChan
  { cClosed :: TVar Bool
  , cChan   :: TChan (Content a)
  }

data Content a = Value a
               | End

{-
 - Functions as STM actions
 -}

newCloseableChanSTM :: STM (CloseableChan a)
newCloseableChanSTM = do
  cClosed <- newTVar False
  cChan   <- newTChan
  return $ CloseableChan{..}

writeChanSTM :: CloseableChan a -> a -> STM (Maybe ())
writeChanSTM CloseableChan{..} a = do
  closed <- readTVar cClosed
  if closed
    then return Nothing
    else Just <$> writeTChan cChan (Value a)

readChanSTM :: CloseableChan a -> STM (Maybe a)
readChanSTM CloseableChan{..} = do
  closed <- readTVar cClosed
  if closed
    then return Nothing
    else Just <$> readValue
  where
    readValue = do
      val <- readTChan cChan
      case val of
        End     -> readValue -- ignore End while reading normally
        Value v -> return v

closeChanSTM :: CloseableChan a -> STM ()
closeChanSTM CloseableChan{..} = do
  writeTVar cClosed True
  --writeTChan cChan End

emptyChanSTM :: CloseableChan a -> STM [a]
emptyChanSTM CloseableChan{..} = do
  writeTChan cChan End
  extractValues
  where
    extractValues = do
      val <- readTChan cChan
      case val of
        End     -> return []
        Value v -> (v :) <$> extractValues

{-
 - Functions as IO actions
 -}

newCloseableChan :: IO (CloseableChan a)
newCloseableChan = atomically newCloseableChanSTM

writeChan :: CloseableChan a -> a -> IO (Maybe ())
writeChan chan = atomically . writeChanSTM chan

readChan :: CloseableChan a -> IO (Maybe a)
readChan = atomically . readChanSTM

closeChan :: CloseableChan a -> IO ()
closeChan = atomically . closeChanSTM

emptyChan :: CloseableChan a -> IO [a]
emptyChan = atomically . emptyChanSTM
