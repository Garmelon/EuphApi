import qualified EuphApi.Connection as E

main = do
  con <- E.startEuphConnection "euphoria.io" "test"
  printEvents con
  where
    printEvents con = do
      event <- E.getEvent con
      case event of
        Just e  -> print e >> putStrLn "" >> printEvents con
        Nothing -> putStrLn "[] end of events"
