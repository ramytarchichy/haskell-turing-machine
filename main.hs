module Main (main) where

import Turing
import LoadTuring
import System.IO

-- Initial input, file import and parsing
main :: IO ()
main = do
    putStr "Please enter a file path: "
    hFlush stdout
    filePath <- getLine
    contents <- readFile filePath
    mainLoop $ turingFromText contents

-- Main program loop fuction
mainLoop :: Turing String Char -> IO ()
mainLoop t = do
    putStrLn $ "State: " ++ state t
    putStrLn $ toList t
    putStrLn $ replicate (headPosition t) ' ' ++ "^"
    if not $ halted t
        then mainLoop $ step t
        else putStrLn "End"
