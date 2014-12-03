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

    imperative :: [(Ident, CoreExpression Machine)] -> [Ident] -> [CoreStatement Machine] -> Subprogram Machine
    imperative locals params stmts = Imperative $ Method locals params stmts

    iAssignment :: Ident -> CoreExpression Machine -> CoreStatement Machine
    iAssignment = (:=)

    iIf :: CoreExpression Machine -> [CoreStatement Machine] -> [CoreStatement Machine] -> CoreStatement Machine
    iIf = IIf 

    iWhile = IWhile 

    iReturn = IReturn 

    iCall = ICall 

    iTrackBack = TrackBack

    iVar = IVar 

    iVal = IVal 

    iOp = IOp 

    iFunc = IFunc

    functional :: CoreLambda Machine -> Subprogram Machine 
    functional expr = Functional expr 

    fVar = FVar 

    fConst = FConst 

    fIf = FIf 

    fOp = FOp 

    fCall = FCall 

    fLet = FLet

    fApp = FApp 

    fAbs = FAbs 

    query :: [Ident] -> Maybe Ident -> [Ident] -> [CoreClause] -> Subprogram Machine
    query params sought bases clause = Logical $ Query params sought bases clause 

    lOr = LOr 

    lNot = LNot 

    lGoal = LGoal 

    lPred = LPred 

    lUnify = (:=:)

    lAtom = Atom 

    lFun ident args = LFun ident (length args) args

    base :: [CoreRule] -> Subprogram Machine
    base = Base

    lRule = (:-)

    bNull = Null

    bBool = Boolean

    bNat = Natural

    bReal = Real 

    bStruct name bscs = Struct name (length bscs) bscs