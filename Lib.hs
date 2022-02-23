{-
Library of generally useful function and datatype definitions.

Author: Patrik Nemeth
Xlogin: xnemet04
Email:  xnemet04@stud.fit.vutbr.cz
-}
module Lib where

import Data.List
import System.IO

-- Shorthand aliases
noNumbering = (-1)
emptySymbol = (Symbol '#' noNumbering)

-- Accessors for a triple
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thr3 :: (a, b, c) -> c
thr3 (_, _, x) = x

data Symbol = Symbol Char Int deriving (Show, Ord, Eq)

-- PLG == Prava Linearni Gramatika
-- For the sake of consistency, this will not be translated.
data PLG = PLG {
    nterms :: [Symbol],  -- Non-terminals
    terms :: [Symbol],   -- Terminals
    start :: Symbol,     -- Starting non-terminal
    rules :: [(Symbol, [Symbol])]   -- Rules of the PLG
} deriving (Show)

-- NKA == Nedeterministicky Konecny Automat
data NKA = NKA {
    states :: [Int],    -- States of the state automata
    symbols :: [Char],  -- Alphabet symbols
    startState :: Int,  -- Starting state
    endStates :: [Int], -- End states
    transitions :: [(Int, Char, Int)] -- Transition rules
}

-- TODO a deduplication function, where Ord is not needed - i.e. without sorting
{-
Deduplicate items in a list. The list is returned sorted.
-}
deDuplicate :: (Eq a, Ord a) => [a] -> [a]
deDuplicate list = deDuplicate' (sort list) []
    where   deDuplicate' [] _ = []
            deDuplicate' (l:ls) dd
                | not $ (l `elem` dd)   = l:(deDuplicate' ls (l:dd))
                | otherwise             = deDuplicate' ls (l:dd)

{-
Promote a Char to a Symbol and return a new list with this new symbol
appended to the second argument of this function.
If a Char already exists in the provided list, then the new symbol will be numbered
one number higher than the highest numbered same Symbol (i.e. if the input
list is [A, A0], return [A, A0, A1]).
-}
addCharAsSymbol :: Char -> [Symbol] -> [Symbol]
addCharAsSymbol c nts = case getHighestSymbolNumber c nts of
                        Left _  -> addSymbol (Symbol c noNumbering) nts  -- Append new Symbol
                        Right i -> addSymbol (Symbol c (i+1)) nts -- For the existing Symbol with this letter add a new Symbol with one higher count
    where   addSymbol new []          = new:[]
            addSymbol new (curr:rest) = curr:(addSymbol new rest)

{-
Return the highest assigned number to a non-terminal.
Non-terminals get numbered during the simplification process.
Returns (Left 0) if the nterm is not found,
otherwise returns (Right i), where i is the highest numbering the given non-terminal has,
where the value (-1) means that the non-terminal exists, but is not assigned a number.
-}
getHighestSymbolNumber :: Char -> [Symbol] -> Either Int Int
getHighestSymbolNumber c nts = getHighestSymbolNumber' c (reverse $ sort nts)
    where getHighestSymbolNumber' _ [] = Left 0
          getHighestSymbolNumber' c ((Symbol nt i):rest)
            | c == nt   = Right i
            | otherwise = getHighestSymbolNumber' c rest

{-
Convert a list of Symbols to a string.
-}
convertSymbolsToChars :: [Symbol] -> [Char]
convertSymbolsToChars [] = []
convertSymbolsToChars ((Symbol c num):[])
    | num >= 0  = (c:(show num))
    | otherwise = c:[]
convertSymbolsToChars ((Symbol c num):ss)
    | num >= 0  = (c:(show num)) ++ (convertSymbolsToChars ss)
    | otherwise = (c:"") ++ (convertSymbolsToChars ss)
