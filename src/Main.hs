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

    import Control.Monad (when)

    import qualified Paths_grammata as Paths (version)

    data Flags = Flags {
        version :: Bool,
        help :: Bool,
        exec :: Bool
        }

    defaultFlags :: Flags
    defaultFlags = Flags {
        version = False,
        help = False,
        exec = True
        }

    header :: String
    header = "grammata script interpreter, version " ++ showVersion Paths.version ++ "\nCopyright (C) 2014 Sascha Rechenberger"

    gnu :: String
    gnu = "This program comes with ABSOLUTELY NO WARRANTY.\n" ++
        "This is free software, and you are welcome to redistribute it under certain conditions. \n" ++
        "For details read the GNU GPLv3 in the LICENSE file or at http://www.gnu.org/licenses/gpl-3.0.txt."


    flags :: [OptDescr (Flags -> Flags)]
    flags = [
        Option "a?" ["about"]
            (NoArg (\ opts -> opts { version = True, exec = False }))
            "show version number",
        Option "h" ["help"]
            (NoArg (\ opts -> opts { help = True, exec = False }))
            "show usage info"
        ]

    -- |@main@ function.
    main :: IO ()
    main = do 
        args <- getArgs
        case getOpt Permute flags args of
            (o,file,[]) -> do
                let opts = (foldl (flip id) defaultFlags o) 
                when (version opts) (putStrLn $ header ++ "\n\n" ++ gnu ++ "\n")
                when (help opts) (putStrLn $ usageInfo header flags)
                when (exec opts) $ case file of
                    file:_ -> if ".gr" `elem` tails file 
                        then readFile file >>= runScript >>= putStrLn
                        else putStrLn $ "ERROR " ++ file ++ "is no *.gr file."
                    _      -> putStrLn $ "ERROR no file" 
            (_,_,err)   -> putStrLn $ usageInfo header flags




