{-|
Module      : Main
Description : grammata-Script Main-Module
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : experimental
Portability : portable
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3

This file is part of grammata.

grammata is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

grammata is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with grammata. If not, see <http://www.gnu.org/licenses/>.
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

