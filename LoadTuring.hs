module LoadTuring (
    turingFromText
) where

{-
 - This file contains all the functions required to parse the text files given
 - as part of the assignment into a usable data structure we can work with.
 -}

import Turing
import qualified Data.Map as Map

-- Splits the text into words
turingFromText :: String -> Turing String Char
turingFromText = turingFromWords . words

-- Creates a Turing Machine
turingFromWords :: [String] -> Turing String Char
turingFromWords (t:i:xs) = turing t ((read i :: Int)-1) "0"
    (\ a b -> Map.lookup (a, b) (programFromWords xs)) 'b'

-- Takes a bunch of lines and puts them in a Map for quick and easy lookup
programFromWords :: [String] -> Map.Map (String, Char) (String, Char, Movement)
programFromWords [] = Map.empty
programFromWords (cS:cV:nV:m:nS:xs) =
    Map.insert (cS, head cV) (nS, head nV, readMovement m) (programFromWords xs)

-- Convert a String to a Movement object
readMovement :: String -> Movement
readMovement "L" = MoveLeft
readMovement "R" = MoveRight
