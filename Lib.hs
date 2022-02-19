{-
Library of generally useful function and datatype definitions.

Author: Patrik Nemeth
Xlogin: xnemet04
Email:  xnemet04@stud.fit.vutbr.cz
-}
module Lib where

import System.IO

-- Accessors for a triple
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thr3 :: (a, b, c) -> c
thr3 (_, _, x) = x

-- PLG == Prava Linearni Gramatika
-- For the sake of consistency, this will not be translated.
data PLG = PLG {
    nterms :: [Char], -- Non-terminals
    terms :: [Char],  -- Terminals
    start :: Char,    -- Starting non-terminal
    rules :: [(Char, [Char])]   -- Rules of the PLG
} deriving (Show)

-- NKA == Nedeterministicky Konecny Automat
data NKA = NKA {
    states :: [Int],    -- States of the state automata
    symbols :: [Char],  -- Alphabet symbols
    startState :: Int,  -- Starting state
    endStates :: [Int], -- End states
    transitions :: [(Int, Char, Int)] -- Transition rules
}