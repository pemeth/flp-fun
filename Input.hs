{-
Functions and datatypes for handling input parsing.

Author: Patrik Nemeth
Xlogin: xnemet04
Email:  xnemet04@stud.fit.vutbr.cz
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
    ntermsLine <- hGetLine fh
    let nterms = parseCommaSeparatedChars ntermsLine

    if not $ foldl (&&) True (map isUpper nterms) then
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

    let ntermsDedup = deDuplicate nterms
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

    return (PLG ntermsDedup termsDedup startNterm rulesDedup)

{-
Check if the defined rules are only using previously defined symbols.
-}
rulesUsingDefinedSymbols :: [(Char, [Char])] -> [Char] -> Bool
rulesUsingDefinedSymbols rules nterms = leftSide rules && rightSide rules
    where   leftSide []         = True
            leftSide (r:rs)     = (fst r) `elem` nterms && leftSide rs
            rightSide []        = True
            rightSide (r:rs)    = all (`elem` nterms) (snd r) && rightSide rs

-- TODO a deduplication function, where Ord is not needed
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
            parseRuleRightSide "#" = ['#']
            parseRuleRightSide (s:"")
                | isUpper s = s:[]
                | otherwise = error ("Right side of rule '" ++ [c,'-','>'] ++ rest ++ "' does not end in a non-terminal")
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