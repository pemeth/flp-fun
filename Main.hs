{-
Program entrypoint.

Author: Patrik Nemeth
Xlogin: xnemet04
Email:  xnemet04@stud.fit.vutbr.cz
-}
import Data.List
import System.Environment
import System.IO

import Input
import Lib

main = do
    (opt, file) <- getArgs >>= argParse

    plg <-  if null file then
                readPLG stdin
            else
                withFile file ReadMode readPLG

    case opt of Print -> printPLG plg
    -- TODO case for Opt1 and Opt2

{-
Print out the PLG as defined for the `-i` program option.
-}
printPLG :: PLG -> IO ()
printPLG plg = do
    let ntermsPrintable = makePrintableSymbols $ (nterms plg)
    let termsPrintable = makePrintableSymbols $ (terms plg)
    let startPrintable = ((start plg):"")
    let rulesPrintable = makePrintableRules (rules plg)

    putStrLn ntermsPrintable
    putStrLn termsPrintable
    putStrLn startPrintable
    printRules rulesPrintable

    where   makePrintableSymbols (s:[]) = s:""
            makePrintableSymbols (s:ss) = s:',':(makePrintableSymbols ss)
            makePrintableRules ((left,right):[]) = ((left:"") ++ "->" ++ right):[]
            makePrintableRules ((left,right):rest) = ((left:"") ++ "->" ++ right):(makePrintableRules rest)
            printRules (r:[]) = putStrLn r
            printRules (r:rs) = do
                putStrLn r
                printRules rs