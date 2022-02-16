module Input where

import System.Console.GetOpt

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