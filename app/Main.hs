module Main where

import CommandHelper

mainLoop :: IO ()
mainLoop = do
    putStrLn "Wpisz komendę..."
    result <- readCommand
    if result
      then mainLoop
      else return ()

main :: IO ()
main = mainLoop


