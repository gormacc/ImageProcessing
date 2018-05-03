module CommandHelper where

processCommand :: String -> IO Bool
processCommand "test" = putStrLn "Wiadomość testowa..." >> return True
processCommand "exit" = putStrLn "Koniec programu..." >> return  False
processCommand _ = putStrLn "Nieznana komenda ..." >> return True

readCommand :: IO Bool
readCommand = getLine >>= processCommand

