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
    import System.Console.GetOpt

    import Data.List (tails)
    import Data.Version (showVersion)

    import qualified Paths_grammata as Paths (version)

    data Flags = Flags {
        version :: Bool
        }

    defaultFlags :: Flags
    defaultFlags = Flags {
        version = False
        }

    flags :: [OptDescr (Flags -> Flags)]
    flags = [
        Option "v?" ["version"]
            (NoArg (\ opts -> opts { version = True }))
            "show version number"
        ]

    -- |@main@ function.
    main :: IO ()
    main = do 
        args <- getArgs
        case getOpt Permute flags args of
            (o,file,[])   -> if version (foldl (flip id) defaultFlags o) 
                then 
                    let 
                        info :: String
                        info = "grammata version " ++ showVersion Paths.version ++ " Copyright (C) 2014  Sascha Rechenberger\n" ++
                            "This program comes with ABSOLUTELY NO WARRANTY.\n" ++
                            "This is free software, and you are welcome to redistribute it under certain conditions. \n" ++
                            "For details read the LICENSE file.\n"
                    in putStrLn info
                else case file of
                    file:_ -> if ".gr" `elem` tails file 
                        then readFile file >>= runScript >>= putStrLn
                        else putStrLn $ "ERROR " ++ file ++ "is no *.gr file." 
            (_,_,err)       -> putStrLn (usageInfo ("grammata version " ++ showVersion Paths.version ++ " Copyright (C) 2014 Sascha Rechenberger\n") flags)




