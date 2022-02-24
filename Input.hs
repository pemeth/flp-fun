{-
Functions and datatypes for handling input parsing.

Project:    plg-2-nka
Year:       2021/2022
Author:     Patrik Nemeth
Xlogin:     xnemet04
Email:      xnemet04@stud.fit.vutbr.cz
-}
module Input where

import System.Console.GetOpt
import System.IO
import Data.Char
import Data.List

import Lib

data ProgOpt = Print | Opt1 | Opt2 deriving (Show, Eq)

options =
    [ Option ['i'] [] (NoArg Print) "Print out the loaded grammar"
    , Option ['1'] [] (NoArg Opt1) "Print out the grammar based on theorem 3.2 from TIN"
    , Option ['2'] [] (NoArg Opt2) "Print out the finite state automata based on theorem 3.6 from TIN"
    ]

{-
Parse input arguments and return appropriately.
-}
argParse :: [String] -> IO (ProgOpt, String)
argParse argv =
    case getOpt Permute options argv of
        ((o:[]), (n:[]), [])    -> return (o,n)     -- One argument, one filename
        ((o:[]), [], [])        -> return (o,"")    -- One argument, no filename
        (_, _, _)               -> ioError (userError (usageInfo header options))
    where header = "\nUsage: ./flp21-fun opts [input]\n\n\
                    \where [input] may be a file, or if ommitted, it is read from stdin.\n"

{-
Take a file handle and extract the PLG.
-}
readPLG :: Handle -> IO (PLG)
readPLG fh = do
    -- TODO check after each line if there is an EOF - if the input is incomplete, parsing crashes
    ntermsLine <- hGetLine fh
    let ntermsChar = parseCommaSeparatedChars ntermsLine

    if not $ foldl (&&) True (map isUpper ntermsChar) then
        error "Non-terminals must be uppercase"
    else
        return ()

    termsLine <- hGetLine fh
    let terms = parseCommaSeparatedChars termsLine

    if not $ foldl (&&) True (map isLower terms) then
        error "Terminals must be lowercase"
    else
        return ()

    startNtermLine <- hGetLine fh
    let startNterm = parseStartingNterm startNtermLine

    if not $ isAlpha startNterm then
        error "Starting non-terminal invalid"
    else
        return ()

    rules <- parseRules fh

    let ntermsDedup = deDuplicate ntermsChar
    let termsDedup = deDuplicate terms
    let rulesDedup = deDuplicate rules

    if not $ rulesUsingDefinedSymbols rulesDedup ('#':(ntermsDedup ++ termsDedup)) then
        error "At least one rule is using an undefined symbol"
    else
        return ()

    if not $ startNterm `elem` ntermsDedup then
        error "The starting non-terminal is not defined"
    else
        return ()

    let startNterm' = (Symbol startNterm noNumbering)

    return (PLG (convertCharsToSymbols ntermsDedup) (convertCharsToSymbols termsDedup) startNterm' (convertRules rulesDedup))

{-
Take an arbitrary list of Chars and promote it to a list of Symbols.
All the Symbols will have -1 as their numbering - i.e. no numbering is applied.
-}
convertCharsToSymbols :: [Char] -> [Symbol]
convertCharsToSymbols [] = []
convertCharsToSymbols (c:cs) = (Symbol c noNumbering):(convertCharsToSymbols cs)

{-
Convert Char based rules to a Symbol based representation.
Similar to `convertCharsToSymbols`.
-}
convertRules :: [(Char, [Char])] -> [(Symbol, [Symbol])]
convertRules [] = []
convertRules ((crLeft, crRight):crs) = ((Symbol crLeft noNumbering, (convertCharsToSymbols crRight))):(convertRules crs)

{-
Check if the defined rules are using only previously defined symbols.
-}
rulesUsingDefinedSymbols :: [(Char, [Char])] -> [Char] -> Bool
rulesUsingDefinedSymbols rules nterms = leftSide rules && rightSide rules
    where   leftSide []         = True
            leftSide (r:rs)     = (fst r) `elem` nterms && leftSide rs
            rightSide []        = True
            rightSide (r:rs)    = all (`elem` nterms) (snd r) && rightSide rs

{-
Parse the rules from Handle 'fh', one per each line.
-}
parseRules :: Handle -> IO [(Char, [Char])]
parseRules fh = do
    isEOF <- hIsEOF fh

    if not isEOF then do
        ruleLine <- hGetLine fh
        nextRule <- parseRules fh
        return ((parseRule ruleLine):nextRule)
    else
        return []

{-
Parse a single PLG rule string. Checks for valid PLG rule syntax,
but does not check if the terminals and non-terminals are valid
i.e. if they have been declared on the first two lines of the PLG definition file.
-}
parseRule :: String -> (Char, [Char])
parseRule (c:'-':'>':rest)
    | isUpper c && (not $ isSimple rest) = (c, parseRuleRightSide rest)
    | isLower c = error ("Left side of rule '" ++ [c,'-','>'] ++ rest ++ "' is a terminal")
    | otherwise = error ("Rule '" ++ [c,'-','>'] ++ rest ++ "' not valid")
    where   isSimple (c:"") = isUpper c
            isSimple _      = False
            parseRuleRightSide ""  = error ("Right side of rule '" ++ [c,'-','>'] ++ rest ++ "' is empty")
            parseRuleRightSide "#" = ['#']
            parseRuleRightSide (s:"")
                | isAlpha s = s:[]
                | otherwise = error ("Right side of rule '" ++ [c,'-','>'] ++ rest ++ "' has a non-terminal at the end")
            parseRuleRightSide (s:ss)
                | isLower s = s:(parseRuleRightSide ss)
                | isUpper s = error ("Right side of rule '" ++ [c,'-','>'] ++ rest ++ "' has a non-terminal elsewhere than at the end")
                | otherwise = error ("Rule '" ++ [c,'-','>'] ++ rest ++ "' is invalid")
parseRule rule = error ("Rule '" ++ rule ++ "' not valid")


{-
Take a String and check if it has one uppercase letter.
If it does, return the letter, if not return a '+' character.
-}
parseStartingNterm :: String -> Char
parseStartingNterm (c:"")
    | isUpper c = c
    | otherwise = '+'
parseStartingNterm _ = '+'

{-
Parse a string, in which there should only be letters separated by commas
in an alternating fashion, starting and ending with a letter.
If any of these constraints is broken, parsing is interrupted and
an array ending with '+' is returned.
-}
parseCommaSeparatedChars :: String -> [Char]
parseCommaSeparatedChars "" = []
parseCommaSeparatedChars "," = ['+']
parseCommaSeparatedChars (_:',':"") = ['+']
parseCommaSeparatedChars (c:',':rest)
    | isAlpha c = c:(parseCommaSeparatedChars rest)
    | otherwise = ['+']
parseCommaSeparatedChars (c:"") = c:[]
parseCommaSeparatedChars _ = ['+']