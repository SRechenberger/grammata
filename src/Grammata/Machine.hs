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
    lOr, lNot, lGoal, lUnify, lFun, lAtom, lRule, lPred, lVar,
    -- ** Basic value core generation.
    bBool, bNat, bReal, bStruct, bNull, Basic (Boolean, Real, Natural)
)
where

    import Grammata.Machine.Core 

    imperative :: ()
        => [(Ident, CoreExpression m)]
        -> [Ident]
        -> [CoreStatement m]
        -> Subprogram m
    imperative locals params stmts = Imperative $ Method locals params stmts

    iAssignment :: ()
        => Ident 
        -> CoreExpression m 
        -> CoreStatement m
    iAssignment = (:=)

    iIf :: () 
        => CoreExpression m 
        -> [CoreStatement m]
        -> [CoreStatement m] 
        -> CoreStatement m
    iIf = IIf 

    iWhile :: () 
        => CoreExpression m 
        -> [CoreStatement m] 
        -> CoreStatement m
    iWhile = IWhile 

    iReturn :: ()  
        => CoreExpression m 
        -> CoreStatement m
    iReturn = IReturn 

    iCall :: ()
        => Ident 
        -> [CoreExpression m] 
        -> CoreStatement m
    iCall = ICall 

    iTrackBack :: () 
        => CoreStatement m
    iTrackBack = ITrackBack

    iVar :: ()
        => Ident 
        -> CoreExpression m 
    iVar = IVar 

    iVal :: ()
        => Basic 
        -> CoreExpression m
    iVal = IVal 

    iOp :: () 
        => ([Basic] -> m Basic) 
        -> [CoreExpression m] 
        -> CoreExpression m
    iOp = IOp 

    iFunc :: () 
        => Ident 
        -> [CoreExpression m] 
        -> CoreExpression m
    iFunc = IFunc

    functional :: ()
        => CoreLambda m 
        -> [Ident] 
        -> Subprogram m
    functional expr idents = Functional (CLM expr idents) 

    fVar :: ()
        => Ident 
        -> CoreLambda m
    fVar = FVar 

    fConst :: ()
        => Basic 
        -> CoreLambda m
    fConst = FConst 

    fIf :: () 
        => CoreLambda m 
        -> CoreLambda m 
        -> CoreLambda m 
        -> CoreLambda m
    fIf = FIf 

    fOp :: ()
        => ([Basic] -> m Basic) 
        -> [CoreLambda m] 
        -> CoreLambda m
    fOp = FOp 

    fCall = FCall 

    fLet = FLet

    fApp = FApp 

    fAbs = FAbs 

    query params sought bases clause = Logical $ Query params sought bases clause 

    lOr = LOr 

    lNot = LNot 

    lGoal = LGoal 

    lPred = LPred 

    lUnify = (:=:)

    lAtom = Atom 

    lFun ident args = LFun ident (length args) args

    base = Base

    lRule = (:-)

    bNull = Null

    bBool = Boolean

    bNat = Natural

    bReal = Real 

    bStruct name bscs = Struct name (length bscs) bscs