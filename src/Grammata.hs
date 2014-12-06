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
-- | Module : Grammata
-- Description : Grammata Main Module
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
---------------------------------------------------------------------------

module Grammata 
(
    executeScript
)
where

    import Grammata.Language (parseGrammata)
    import Grammata.Interpreter (compileGrammata)
    import Grammata.Machine (runProgram)

    executeScript :: () 
        => String 
        -> IO ()
    executeScript script = case parseGrammata script of
            Left err  -> putStrLn $ "PARSER ERROR " ++ err 
            Right ast -> case compileGrammata ast of 
                    Left err  -> putStrLn $ "COMPILER ERROR " ++ err 
                    Right (subprgs, globs) -> runProgram subprgs globs