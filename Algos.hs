{-
Algorithms for transforming the input PLG to required outputs.

Author: Patrik Nemeth
Xlogin: xnemet04
Email:  xnemet04@stud.fit.vutbr.cz
-}

module Algos where

import Data.List

import Lib

-- TODO create my own elemIndex that does not return Maybe Int, since I don't need to
--      check if a value is in the array - that is guaranteed by the input parser...
{-
A simplified PLG (option '-1' of the application) is translated to an NKA.
Assumes that the input PLG is in its simplified form, undefined behaviour otherwise.
-}
plgSimple2nka :: PLG -> NKA
plgSimple2nka plg = NKA q s strt (ends $ rules plg) (trans $ rules plg)
    where   q = [i | let l = length (nterms plg) - 1, i <- [0..l]]
            s = convertSymbolsToChars (terms plg)
            strt = elemIndex' (start plg) (nterms plg)
            ends []                               = []
            ends ((leftSide, [emptySymbol]):[])   = (elemIndex' leftSide (nterms plg)):[]
            ends ((leftSide, [emptySymbol]):rest) = (elemIndex' leftSide (nterms plg)):(ends (rest))
            ends (_:rest)                         = ends rest
            trans []                        = []
            trans ((_, [emptySymbol]):rest) = trans rest
            trans ((leftSide, a:b:[]):rest) = (initialState, transSymb a, resultingState):(trans rest)
                where   initialState           = elemIndex' leftSide (nterms plg)
                        resultingState         = elemIndex' b (nterms plg)
                        transSymb (Symbol c _) = c
            trans _                         = error "This should absolutely never happen, something is wrong with the transformation of PLG into its simpler form"
            elemIndex' elem list = case elemIndex elem list of
                                    Nothing  -> error "This should never happen"
                                    Just idx -> idx