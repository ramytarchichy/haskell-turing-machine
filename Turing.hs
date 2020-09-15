module Turing (
    Turing(..),
    Movement(..),
    turing,
    toList,
    headPosition,
    step,
    halted
) where

{-
 - This file contains the core functionality of the program, which is simulating
 - a Turing Machine.
 -}

-- Indicates wether the head/tape should move to the left or to the right
data Movement = MoveLeft | MoveRight

-- This holds the machine state, tape (split to the left and right of the head),
-- transition function, etc.
data Turing s t = Turing {
    value :: t,
    left :: [t],
    right :: [t],
    defaultValue :: t,

    state :: s,
    transitionFunction :: s -> t -> Maybe (s, t, Movement)
}


-- Creates turing machine from a list of items (tape) and head position
turing :: [t] -> Int -> s -> (s -> t -> Maybe (s, t, Movement)) -> t
    -> Turing s t
turing t i s f d = Turing {
    value=head $ snd $ splitAt i t, left=fst $ splitAt i t,
    right=tail $ snd $ splitAt i t, defaultValue=d, state=s,
    transitionFunction=f
}


-- Pattern matching could be used here, but I opted to go for this syntax
-- as it avoids problems if I decide to modify the data structure
moveRight :: Turing s t -> Turing s t
moveRight x = let y = x{left=left x ++ [value x]} in case right x of
    [] -> y{value=defaultValue x}
    r  -> y{value=head r, right=tail r}

-- Same here
moveLeft :: Turing s t -> Turing s t
moveLeft x = let y = x{right=value x : right x} in case left x of
    [] -> y{value=defaultValue x}
    l  -> y{value=last l, left=init l}

-- Joins the split tape and returns as a single list for easy manipulation
toList :: Turing s t -> [t]
toList x = left x ++ [value x] ++ right x

-- Gives the head position, starting from 0 on the leftmost side of the tape.
-- Does not go negative, instead the tape extends to the left and this stays 0
-- if the head goes too far to the left.
headPosition :: Turing s t -> Int
headPosition = length . left

-- Calculates the next machine state through the transition function.
step :: Turing s t -> Turing s t
step x = case transitionFunction x (state x) (value x) of
    Nothing -> x
    Just (newState, newValue, movement) ->
        let y = x{value=newValue, state=newState} in case movement of
            MoveLeft -> moveLeft y
            MoveRight -> moveRight y

-- Is true if the machine has reached a halting state
halted :: Turing s t -> Bool
halted x = case transitionFunction x (state x) (value x) of
    Nothing -> True
    _ -> False
