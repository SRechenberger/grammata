{-|
Module      : Main
Description : grammata-Script Main-Module
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : experimental
Portability : portable
-}

module Main 
(   
    main
)
where
    
    import Grammata (runScript)

    import System.Environment (getArgs)

    import Data.List (tails)

    -- |@main@ function.
    main :: IO ()
    main = do 
        args <- getArgs
        case args of
            []  -> putStrLn "ERROR no input file"
            t:_ -> if ".gr" `elem` tails t 
                then readFile t >>= runScript >>= putStrLn
                else putStrLn "ERROR no *.gr file."

