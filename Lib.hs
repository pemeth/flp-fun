{-
Library of generally useful function and datatype definitions.

Author: Patrik Nemeth
Xlogin: xnemet04
Email:  xnemet04@stud.fit.vutbr.cz
-}
module Lib where

import System.IO

-- PLG == Prava Linearni Gramatika
-- For the sake of consistency, this will not be translated.
data PLG = PLG {
    nterm :: [Char], -- Non-terminals
    term :: [Char],  -- Terminals
    start :: Char,   -- Starting non-terminal
    rules :: [(Char, [Char])]   -- Rules of the PLG
} deriving (Show)