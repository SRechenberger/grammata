---------------------------------------------------------------------------
-- This file is part of grammata.
-- 
-- grammata is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- grammata is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with grammata. If not, see <http://www.gnu.org/licenses/>.
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Module : Main
-- Description : Entry Module
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014, 2015
-- License : GPL-3
---------------------------------------------------------------------------

module Main 
(
    main
) 
where

    import System.IO
    import System.Environment (getArgs)

    import Data.List (isSuffixOf)

    import Grammata (executeScript)

    -- | Entry point.
    main :: IO ()
    main = do 
        args <- getArgs 
        case args of 
    	   [] -> putStrLn "no file to execute..."
    	   file:_  
                | ".gr" `isSuffixOf` file -> do
                    hSetBuffering stdin NoBuffering
                    script <- readFile file
                    executeScript script 
                | otherwise -> putStrLn $ file ++ " is no *.gr file..."
