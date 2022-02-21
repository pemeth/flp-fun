{-
Program entrypoint.

Author: Patrik Nemeth
Xlogin: xnemet04
Email:  xnemet04@stud.fit.vutbr.cz
-}
import Data.List
import System.Environment
import System.IO

import Algos
import Input
import Lib

main = do
    (opt, file) <- getArgs >>= argParse

    plg <-  if null file then
                readPLG stdin
            else
                withFile file ReadMode readPLG

    case opt of Print -> printPLG plg
                Opt2  -> printNKA $ plgSimple2nka plg
    -- TODO case for Opt1

{-
Print out the PLG as defined for the `-i` program option.
-}
printPLG :: PLG -> IO ()
printPLG plg = do
    let ntermsPrintable = makePrintableSymbols $ nterms plg
    let termsPrintable = makePrintableSymbols $ terms plg
    let startPrintable = makePrintableStart $ start plg
    let rulesPrintable = makePrintableRules $ rules plg

    putStrLn ntermsPrintable
    putStrLn termsPrintable
    putStrLn startPrintable
    printRules rulesPrintable

    where   makePrintableSymbols [] = ""
            makePrintableSymbols ((Symbol c num):[])
                | num >= 0  = (c:(show num))
                | otherwise = c:""
            makePrintableSymbols ((Symbol c num):ss)
                | num >= 0  = (c:(show num)) ++ "," ++ (makePrintableSymbols ss)
                | otherwise = (c:",") ++ (makePrintableSymbols ss)
            makePrintableRules [] = []
            makePrintableRules (((Symbol c num),right):rest)
                | num >= 0  = ((c:(show num)) ++ "->" ++ (convertSymbolsToChars right)):(makePrintableRules rest)
                | otherwise = ((c:"") ++ "->" ++ (convertSymbolsToChars right)):(makePrintableRules rest)
            makePrintableStart (Symbol c num)
                | num >= 0 = c:(show num)
                | otherwise = c:""
            printRules (r:[]) = putStrLn r
            printRules (r:rs) = do
                putStrLn r
                printRules rs

{-
Print out an NKA.
-}
printNKA :: NKA -> IO ()
printNKA nka = do
    let statesPrintable = makePrintableIntegers (states nka)
    let symbolsPrintable = makePrintableSymbols (symbols nka)
    let startStatePrintable = show $ startState nka
    let endStatesPrintable = makePrintableIntegers (endStates nka)
    let transitionsPrintable = makePrintableTransitions (transitions nka)

    putStrLn statesPrintable
    putStrLn symbolsPrintable
    putStrLn startStatePrintable
    putStrLn endStatesPrintable
    printTransitions transitionsPrintable

    where   makePrintableIntegers [] = ""
            makePrintableIntegers (i:[]) = (show i)
            makePrintableIntegers (i:is) = (show i)++","++(makePrintableIntegers is)
            makePrintableSymbols (s:[]) = s:""
            makePrintableSymbols (s:ss) = s:',':(makePrintableSymbols ss)
            makePrintableTransitions (t:[]) = ((show $ fst3 t) ++ "," ++ (snd3 t:"") ++ "," ++ (show $ thr3 t)):[]
            makePrintableTransitions (t:ts) = ((show $ fst3 t) ++ "," ++ (snd3 t:"") ++ "," ++ (show $ thr3 t)):(makePrintableTransitions ts)
            printTransitions (t:[]) = putStrLn t
            printTransitions (t:ts) = do
                putStrLn t
                printTransitions ts
