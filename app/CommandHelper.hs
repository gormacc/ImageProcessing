module CommandHelper where

import ImageManipulation

askForData :: String -> IO String
askForData prompt = putStrLn prompt >> getLine >>= return

processCommand :: String -> IO Bool
processCommand "test" = putStrLn "Wiadomość testowa..." >> return True
processCommand "rotate" = putStrLn "Obracanie obrazka ..." >> myReadImage rotateImg >> return True
processCommand "exit" = putStrLn "Koniec programu..." >> return  False
processCommand _ = putStrLn "Nieznana komenda ..." >> return True

readCommand :: IO Bool
readCommand = getLine >>= processCommand

