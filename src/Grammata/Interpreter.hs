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
-- | Module : Grammata.Interpreter
-- Description : Grammata Interpreter Module
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
---------------------------------------------------------------------------

module Grammata.Interpreter
(
    compileGrammata
)
where

        import Grammata.Interpreter.Compilation (compileProgram, Program (..))
        import Grammata.Interpreter.Compiler (runCompiler)

        import Grammata.Machine 

        compileGrammata :: ()
            => Program 
            -> Either String ([(Ident, Subprogram Machine)], [(Ident, CoreExpression Machine)])
        compileGrammata prg = runCompiler (compileProgram prg) (fst . unzip . globals $ prg) (fst . unzip . subs $ prg)

