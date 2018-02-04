import           Control.Concurrent
import qualified EuphApi.CloseableChan as E

thread1 c n = do
  E.writeChan c n
  thread1 c (n + 1)

thread2 c = do
  val <- E.readChan c
  case val of
    Just j  -> (print j) >> thread2 c
    Nothing -> putStrLn "thread2 closed!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

main1 = do
  c <- E.newOpenChan
  t1 <- forkIO $ thread1 c 0
  t2 <- forkIO $ thread2 c
  threadDelay $ 500*1000
  E.closeChan c
  threadDelay $ 500*1000

main2 = do
  c <- E.newOpenChan
  forkIO $ thread2 c
  putStrLn "Go!"

  mapM_ (E.writeChan c) [1..5]
  -- putStrLn "added some numbers"

  threadDelay $ 1000*1000
  putStrLn "first second"
  threadDelay $ 1000*1000
  putStrLn "second second"

  mapM_ (E.writeChan c) [6..10]
  -- putStrLn "added more numbers"

  threadDelay $ 1000*1000
  putStrLn "third second"
  threadDelay $ 1000*1000
  putStrLn "fourth second"

  E.closeChan c
  -- putStrLn "channel closed"

  threadDelay $ 1000*1000
  putStrLn "fifth second"
  threadDelay $ 1000*1000
  putStrLn "sixth second"

main = main2
