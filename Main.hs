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

main = do
    (opt, file) <- getArgs >>= argParse

    plg <-  if null file then
                readPLG stdin
            else
                withFile file ReadMode readPLG

    putStrLn $ show plg
    putStrLn $ show opt
    putStrLn file