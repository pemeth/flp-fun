{-
Algorithms for transforming the input PLG to required outputs.

Project:    plg-2-nka
Year:       2021/2022
Author:     Patrik Nemeth
Xlogin:     xnemet04
Email:      xnemet04@stud.fit.vutbr.cz
-}
module Algos where

import Data.Char
import Data.List

import Lib

{-
Simplify a PLG according to theorem 3.2 from TIN.
-}
plg2simple :: PLG -> PLG
plg2simple plg = PLG (deDuplicate nts) ts s (deDuplicate rls)
    where   (rls, nts) = splitRules (rules plg) (nterms plg) []
            ts = terms plg
            s = start plg

{-
Split a list of rules according to theorem 3.2 from TIN.
-}
splitRules :: [(Symbol, [Symbol])] -> [Symbol] -> [(Symbol, [Symbol])] -> ([(Symbol, [Symbol])], [Symbol])
splitRules [] nts accrls = (accrls, nts)
splitRules (rl:rls) nts accrls = splitRules rls newNts (union newRls accrls)
    where   (newRls, newNts) = splitRule rl [] nts

{-
Take a rule and split it according to theorem 3.2 from TIN.
-- TODO maybe rework this function, so that the user does not need to provide an empty accumulator list
The function must be provided with: the rule to be split, an empty accumulator list, PLG non-terminals.
-}
splitRule :: (Symbol, [Symbol]) -> [(Symbol, [Symbol])] -> [Symbol] -> ([(Symbol, [Symbol])], [Symbol])
splitRule (_, []) rls nts = (rls, nts)
splitRule (leftSide, ((Symbol '#' rhi):[])) rls nts = ((leftSide, rightSide):rls, nts)
    where   rightSide = [(Symbol '#' rhi)]
splitRule ((Symbol lhc lhi), ((Symbol rhc rhi):[])) rls nts = ((leftSide, rightSideInter):(interNt, [epsilon]):rls, newNts)
    where   interNt = last $ addCharAsSymbol lhc nts
            leftSide = (Symbol lhc lhi)
            rightSideInter = [(Symbol rhc rhi), interNt]
            newNts = interNt:nts
splitRule ((Symbol lhc lhi), (rhTerm:(Symbol rhc rhi):[])) rls nts
    | isUpper rhc = (((leftSide, [rhTerm, rhNterm])):rls, nts) -- Rightmost symbol is a non-terminal
    | otherwise   = ((leftSide, [rhTerm, interNt]):(interNt, [rhTermFinal, finalNt]):(finalNt, [epsilon]):rls, newNts) -- Rightmost symbol is a terminal
    where   rhNterm = (Symbol rhc rhi)
            leftSide = (Symbol lhc lhi)
            newNts = interNt:finalNt:nts
            rhTermFinal = (Symbol rhc rhi)
            interNt = last $ addCharAsSymbol lhc nts
            finalNt = last $ addCharAsSymbol lhc (interNt:nts)
splitRule (Symbol lhc lhi, ((Symbol rhc rhi):rhs)) rls nts = splitRule (newNt, rhs) ((leftSide, [rightSideFirst, newNt]):rls) (newNt:nts)
    where   leftSide = (Symbol lhc lhi)
            rightSideFirst = (Symbol rhc rhi)
            newNt = last $ addCharAsSymbol lhc nts


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
            ends []                           = []
            ends ((leftSide, [epsilon]):[])   = (elemIndex' leftSide (nterms plg)):[]
            ends ((leftSide, [epsilon]):rest) = (elemIndex' leftSide (nterms plg)):(ends (rest))
            ends (_:rest)                     = ends rest
            trans []                        = []
            trans ((_, [epsilon]):rest)     = trans rest
            trans ((leftSide, a:b:[]):rest) = (initialState, transSymb a, resultingState):(trans rest)
                where   initialState           = elemIndex' leftSide (nterms plg)
                        resultingState         = elemIndex' b (nterms plg)
                        transSymb (Symbol c _) = c
            trans _                         = error "This should absolutely never happen, something is wrong with the transformation of PLG into its simpler form"
            elemIndex' elem list = case elemIndex elem list of
                                    Nothing  -> error "This should never happen"
                                    Just idx -> idx