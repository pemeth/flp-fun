module Input where

import System.Console.GetOpt

data ProgOpt = Print | Opt1 | Opt2 deriving (Show, Eq)

options =
    [
        Option ['i'] [] (NoArg Print),
        Option ['1'] [] (NoArg Opt1),
        Option ['2'] [] (NoArg Opt2)
    ]