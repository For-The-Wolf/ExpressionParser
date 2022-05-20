module Main where

import ExpressionParser.Expr

main :: IO()
main = do
    putStrLn "------------------------------------"
    putStrLn "Input a function or expression:"
    r <- getLine
    if (r == "stop")
        then do
            putStrLn "Exiting..."
            putStrLn "------------------------------------"
        else do
            if (isFunc r)
                then do 
                    let f = str2Func r 
                    let f' = dF f 
                    let f'' = dF f'
                    putStrLn (show f)
                    putStrLn (show f')
                    putStrLn (show f'')
                    putStrLn ("What would you like to substitute for " ++ (getVarName f) ++ "?")
                    rNum <- getDouble
                    let fNum = subNum f rNum
                    let f'Num = subNum f' rNum
                    let f''Num = subNum f'' rNum
                    putStrLn (show fNum)
                    putStrLn (show f'Num)
                    putStrLn (show f''Num)
                else  putStrLn (show (str2Expr r))
            main