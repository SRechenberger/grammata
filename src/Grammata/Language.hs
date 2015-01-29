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
-- | Module : Grammata.Language
-- Description : Grammata Language Definition Module
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014, 2015
-- License : GPL-3
---------------------------------------------------------------------------

module Grammata.Language
(    
    -- * Grammata AST
    Program (..), Value (..), Expression (..), Returns (..),
    -- ** Subprograms
    Subprg (..), 
    -- ** Functional
    Lambda (..),
    -- ** Imperative
    Statement (..),
    -- ** Logical
    Rule (..), Clause (..), Goal (..), Term (..),

    -- * Grammata Parser
    parseGrammata
)
where

    import Grammata.Language.Program (Program (..), Returns (..), Subprg (..), parseProgram)
    import Grammata.Language.Functional (Lambda (..))
    import Grammata.Language.Imperative (Statement (..))
    import Grammata.Language.Logical (Term (..), Goal (..), Clause (..), Rule (..), Base)
    import Grammata.Language.Value (Value (..))
    import Grammata.Language.Expression (Expression (..))

    import Text.Parsec (parse)

    -- | Parses a grammata program.
    parseGrammata :: String -> Either String Program 
    parseGrammata input = case parse parseProgram "" input of
        Left msg  -> Left . show $ msg
        Right ast -> Right ast 