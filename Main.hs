import System.Environment
import Data.List

import Input

main = do
    (opt, file) <- getArgs >>= argParse
    putStrLn $ show opt
    putStrLn file