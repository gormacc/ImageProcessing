module CommandHelper where

import ImageManipulation

askForData :: String -> IO String
askForData prompt = putStrLn prompt >> getLine >>= return

processCommand :: String -> IO Bool
processCommand "test" = putStrLn "Wiadomość testowa..." >> return True
processCommand "rotate180" = putStrLn "Obracanie obrazka 180 ..." >> myReadImage rotate180 >> return True
processCommand "rotate90" = putStrLn "Obracanie obrazka 90 ..." >> myReadImage rotate90 >> return True
processCommand "rotate270" = putStrLn "Obracanie obrazka 270 ..." >> myReadImage rotate270 >> return True
processCommand "exit" = putStrLn "Koniec programu..." >> return  False
processCommand _ = putStrLn "Nieznana komenda ..." >> return True

readCommand :: IO Bool
readCommand = getLine >>= processCommand

