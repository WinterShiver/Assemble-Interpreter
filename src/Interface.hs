module Interface where 

import Interpreter (interpret)
import System.Environment

main = do {
    args <- getArgs;
    code <- readFile $ head args;
    case interpret code of
        Nothing -> putStrLn "OK"
        Just str -> putStrLn str
}