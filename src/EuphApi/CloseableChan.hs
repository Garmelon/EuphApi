{-# LANGUAGE RecordWildCards #-}

-- | Chans that can be closed and reopened.
--
-- While a 'CloseableChan' is closed, it can not be written to or read from.
-- Calls to 'writeChan' and 'readChan' are non-blocking while a chan is closed.
--
-- If a thread is attempting to read from a chan using 'readChan' and that chan is closed,
-- the call to 'readChan' resumes and @Nothing@ is returned.

module EuphApi.CloseableChan
  ( CloseableChan
  -- * IO function versions
  , newOpenChan
  , newClosedChan
  , writeChan
  , readChan
  , closeChan
  , openChan
  , emptyChan
  -- * STM function versions
  , newOpenChanSTM
  , newClosedChanSTM
  , writeChanSTM
  , readChanSTM
  , closeChanSTM
  , openChanSTM
  , emptyChanSTM
  ) where

import           Control.Concurrent.STM

-- | A 'Chan' that can be closed and opened again.
--
-- Attempts to write to or read from a 'CloseableChan' while it is closed result
-- in a @Nothing@.
data CloseableChan a = CloseableChan
  { cClosed :: TVar Bool
  , cChan   :: TChan (Content a)
  }

-- TODO: Replace with Maybe?
data Content a = Value a
               | End

{-
 - Functions as STM actions
 -}

-- | See 'newOpenChan'.
newOpenChanSTM :: STM (CloseableChan a)
newOpenChanSTM = do
  cClosed <- newTVar False
  cChan   <- newTChan
  return CloseableChan{..}

-- | See 'newClosedChan'.
newClosedChanSTM :: STM (CloseableChan a)
newClosedChanSTM = do
  cClosed <- newTVar True
  cChan   <- newTChan
  return CloseableChan{..}

-- | See 'writeChan'.
writeChanSTM :: CloseableChan a -> a -> STM (Maybe ())
writeChanSTM CloseableChan{..} a = do
  closed <- readTVar cClosed
  if closed
    then return Nothing
    else Just <$> writeTChan cChan (Value a)

-- | See 'readChan'.
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

-- | See 'closeChan'.
closeChanSTM :: CloseableChan a -> STM ()
closeChanSTM CloseableChan{..} = writeTVar cClosed True
  --writeTChan cChan End

-- | See 'openChan'.
openChanSTM :: CloseableChan a -> STM ()
openChanSTM CloseableChan{..} = writeTVar cClosed False

-- | See 'emptyChan'.
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

-- | Create a new open 'CloseableChan'.
newOpenChan :: IO (CloseableChan a)
newOpenChan = atomically newOpenChanSTM

-- | Create a new closed 'CloseableChan'.
newClosedChan :: IO (CloseableChan a)
newClosedChan = atomically newClosedChanSTM

-- | Attempt to write a value into the 'CloseableChan'.
--
-- If the chan is open, succeeds with a @Just ()@.
-- If the chan is closed, fails with a @Nothing@.
writeChan :: CloseableChan a -> a -> IO (Maybe ())
writeChan chan a = atomically $ writeChanSTM chan a

-- | Attempt to read a value @v@ from the 'CloseableChan'.
--
-- If the chan is open, succeeds with a @Just v@.
-- If the chan is closed, fails with a @Nothing@.
readChan :: CloseableChan a -> IO (Maybe a)
readChan = atomically . readChanSTM

-- | Close a 'CloseableChan'.
-- Does nothing if chan is already closed.
--
-- Performing this action un-blocks all calls to 'readChan'.
closeChan :: CloseableChan a -> IO ()
closeChan = atomically . closeChanSTM

-- | Open a 'CloseableChan'.
-- Does nothing if chan is already open.
openChan :: CloseableChan a -> IO ()
openChan = atomically . openChanSTM

-- | Remove all items currently in the 'CloseableChan' and returns them in a list.
--
-- This function also works while the chan is closed.
-- It is meant as a way to clean up the remaining values in a chan after it was closed.
emptyChan :: CloseableChan a -> IO [a]
emptyChan = atomically . emptyChanSTM
