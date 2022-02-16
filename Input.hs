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
    ntermLine <- hGetLine fh
    let nterm = parseCommaSeparatedChars ntermLine

    if not $ foldl (&&) True (map isUpper nterm) then
        error "Non-terminals must be uppercase"
    else
        return ()

    -- TODO finish reading the PLG from the Handle

    return (PLG nterm [] 'a' [])


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