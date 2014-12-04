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
-- | Module : Grammata.Machine
-- Description : Grammata Virtual Machine Module
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
---------------------------------------------------------------------------

module Grammata.Machine
(
    -- * Grammata Machine
    Machine, Ident,

    -- * Core subprogram generation.
    imperative, functional, query, base,
    -- ** Imperative core generation.
    iAssignment, iIf, iWhile, iReturn, iCall, iTrackBack, iVar, iVal, iOp, iFunc, CoreStatement,
    -- ** Functional core generation.
    fVar, fConst, fIf, fOp, fCall, fLet, fApp, fAbs,
    -- ** Logical core generation.
    lOr, lNot, lGoal, lUnify, lFun, lAtom, (<--), lPred, lVar,
    -- ** Basic value core generation.
    bBool, bNat, bReal, bStruct, bNull, Basic (Boolean, Real, Natural)
)
where

    import Grammata.Machine.Core 

    -- | New imperative subprogram.
    imperative :: ()
        => [(Ident, CoreExpression m)]  -- ^ Local variables.
        -> [Ident]                      -- ^ Parameters.
        -> [CoreStatement m]            -- ^ Instructions.
        -> Subprogram m                 -- ^ Imperative subprogram.
    imperative locals params stmts = Imperative $ Method locals params stmts

    -- | Assignment: a := b;
    iAssignment :: ()
        => Ident            -- ^ Identifier a.
        -> CoreExpression m -- ^ Expression b.
        -> CoreStatement m  -- ^ Assignment.
    iAssignment = (:=)

    -- | If-statement: if c then a else b end
    iIf :: () 
        => CoreExpression m     -- ^ Condition c.
        -> [CoreStatement m]    -- ^ then-block a.
        -> [CoreStatement m]    -- ^ else-block b.
        -> CoreStatement m      -- if statement.
    iIf = IIf 

    -- | While-loop: while c do 
    iWhile :: () 
        => CoreExpression m     -- ^ Condition c.
        -> [CoreStatement m]    -- ^ Loop-block.
        -> CoreStatement m      -- ^ while-loop.
    iWhile = IWhile 

    -- | Return statement: return e.
    iReturn :: ()  
        => CoreExpression m     -- ^ expression e.
        -> CoreStatement m      -- ^ return statement.
    iReturn = IReturn 

    -- | Procedure call: call p(a1,a2,..,an).
    iCall :: ()
        => Ident                -- ^ Procedure identifier p.
        -> [CoreExpression m]   -- ^ Argument expressions a1,a2,..an.
        -> CoreStatement m      -- ^ Procedure call.
    iCall = ICall 

    -- | Forced backtracking: trackback.
    iTrackBack :: () 
        => CoreStatement m      -- ^ Trackback statement.
    iTrackBack = ITrackBack

    -- | Variable expression: a.
    iVar :: ()
        => Ident            -- ^ Identifier a.
        -> CoreExpression m -- ^ Variable expression.
    iVar = IVar 

    -- | Constant expression: c.
    iVal :: ()
        => Basic            -- ^ Constant value c.
        -> CoreExpression m -- ^ Constant expression.
    iVal = IVal 

    -- | n-ary operatrion expression: op(a1,a2,..,an).
    iOp :: () 
        => ([Basic] -> m Basic)     -- ^ Operator op.
        -> [CoreExpression m]       -- ^ Operand expressions a1,a2,..,an.
        -> CoreExpression m         -- ^ n-ary operatrion expression.
    iOp = IOp 

    -- | Function call expression: f(a1,a2,..,an).
    iFunc :: () 
        => Ident                -- ^ Function identifier f.
        -> [CoreExpression m]   -- ^ Argument expressions a1,a2,..,an.
        -> CoreExpression m     -- ^ Function call expression.
    iFunc = IFunc

    -- | New functional subprogram.
    functional :: ()
        => CoreLambda m     -- ^ Subprogram expression.
        -> [Ident]          -- ^ Parameters.
        -> Subprogram m     -- ^ Functional subprogram.
    functional expr idents = Functional (CLM expr idents) 

    -- | Functional variable expression: v.
    fVar :: ()
        => Ident        -- ^ Variable identifier v.
        -> CoreLambda m -- ^ Functional variable expression.
    fVar = FVar 

    -- | Functional constant expression: c.
    fConst :: ()
        => Basic        -- ^ Constant value c.
        -> CoreLambda m -- ^ Functional constant expression.
    fConst = FConst 

    -- | Functional if expression: if c then a else b end.
    fIf :: () 
        => CoreLambda m -- ^ Condition expression c.
        -> CoreLambda m -- ^ Then expression a.
        -> CoreLambda m -- ^ Else expression b.
        -> CoreLambda m -- ^ Functional if expression.
    fIf = FIf 

    fOp :: ()
        => ([Basic] -> m Basic) 
        -> [CoreLambda m] 
        -> CoreLambda m
    fOp = FOp 

    fCall :: ()
        => Ident 
        -> [CoreLambda m]
        -> CoreLambda m
    fCall = FCall 

    fLet :: () 
        => [(Ident, CoreLambda m)] 
        -> CoreLambda m
        -> CoreLambda m
    fLet = FLet

    fApp :: ()
        => CoreLambda m 
        -> [CoreLambda m]
        -> CoreLambda m 
    fApp = FApp 

    fAbs :: ()
        => [Ident]
        -> CoreLambda m 
        -> CoreLambda m
    fAbs = FAbs 

    query :: ()
        => [Ident]
        -> Maybe Ident 
        -> [Ident]
        -> [CoreClause]
        -> Subprogram m
    query params sought bases clause = Logical $ Query params sought bases clause 

    lOr :: ()
        => [[CoreClause]] 
        -> CoreClause
    lOr = LOr 

    lNot :: ()
        => [CoreClause]
        -> CoreClause
    lNot = LNot 

    lGoal :: ()
        => CoreGoal
        -> CoreClause
    lGoal = LGoal 

    lPred :: ()
        => Ident
        -> Int 
        -> [CoreTerm]
        -> CoreGoal
    lPred = LPred 

    lUnify :: ()
        => CoreTerm
        -> CoreTerm
        -> CoreGoal
    lUnify = (:=:)

    lAtom :: ()
        => Basic
        -> CoreTerm
    lAtom = Atom 

    lFun :: ()
        => Ident
        -> [CoreTerm]
        -> CoreTerm
    lFun ident args = LFun ident (length args) args

    base :: ()
        => [CoreRule]
        -> Subprogram m
    base = Base

    (<--) :: ()
        => CoreGoal
        -> [CoreClause]
        -> CoreRule
    (<--) = (:-)

    bNull :: ()
        => Basic
    bNull = Null

    bBool :: ()
        => Bool 
        -> Basic
    bBool = Boolean

    bNat :: ()
        => Integer
        -> Basic
    bNat = Natural

    bReal :: ()
        => Double
        -> Basic
    bReal = Real 

    bStruct :: ()
        => String 
        -> [Basic]
        -> Basic
    bStruct name bscs = Struct name (length bscs) bscs