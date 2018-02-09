import qualified EuphApi.Connection as E

main = do
  con <- E.startEuphConnection "euphoria.io" "test"
  printEvents con
  where
    printEvents con = do
      event <- E.getEvent con
      case event of
        E.EuphEvent e -> print e >> putStrLn "" >> printEvents con
        _             -> putStrLn "[] end of events"
