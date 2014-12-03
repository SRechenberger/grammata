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
-- | Module : Grammata.Interpreter.Compilation
-- Description : Grammata AST to Monad Compiler Module
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
---------------------------------------------------------------------------

module Grammata.Interpreter.Compilation
(
    compileProgram
)
where

    import Grammata.Machine (
        imperative, functional, query, base,
        iAssignment, iIf, iWhile, iReturn, iCall, iTrackBack, iVal, iVar, iOp, iFunc, CoreStatement,
        fVar, fConst, fIf, fOp, fCall, fLet, fApp, fAbs,
        lOr, lNot, lGoal, lUnify, lFun, lAtom, lRule, lPred, lVar,
        bBool, bNat, bReal, bStruct, bNull, 
        Machine, Ident
        )
    import qualified Grammata.Machine as M (Basic (Natural, Boolean, Real))
    import Grammata.Language(
        Program (..),
        Value (..),
        Expression (..),
        Returns (..),
        Subprg (..),
        Lambda (..),
        Statement (..),
        Rule (..),
        Clause (..),
        )

    import Grammata.Interpreter.Compiler (Compiler, runCompiler, ask)

    import Control.Applicative ((<$>), (<*>))


   

