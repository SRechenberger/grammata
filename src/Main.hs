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
    
    import Grammata

    import System.Environment

    main :: IO ()
    main = do 
        args <- getArgs
        case args of
            []  -> putStrLn "ERROR no input file"
            t:_ -> do
                readFile t >>= runScript >>= putStrLn
