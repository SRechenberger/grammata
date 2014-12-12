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
    Machine, Ident, runProgram,

    -- * Core subprogram generation.
    imperative, functional, query, base, Subprogram,
    -- ** Imperative core generation.
    iAssignment, iIf, iWhile, iReturn, iCall, iTrackBack, iVar, iVal, iOp, iFunc, CoreStatement, CoreExpression,
    -- ** Functional core generation.
    fVar, fConst, fIf, fOp, fCall, fLet, fApp, fAbs, CoreLambda,
    -- ** Logical core generation.
    lOr, lNot, lGoal, lUnify, lFun, lAtom, lRule, lPred, lVar, CoreClause, CoreRule, CoreGoal, CoreTerm,
    -- ** Basic value core generation.
    bBool, bNat, bReal, bStruct, bNull, Basic (Boolean, Real, Natural), (=:=), (=/=)
)
where

    import Grammata.Machine.Core 

    -- | New imperative subprogram.
    imperative :: ()
        => [(Ident, CoreExpression m)]  -- ^ Local variables.
        -> [Ident]                      -- ^ Parameters.
        -> [CoreStatement m]            -- ^ Instructions.
        -> Bool                         -- ^ True if writing access to global variables is granted.
        -> Subprogram m                 -- ^ Imperative subprogram.
    imperative locals params stmts access = Imperative (Method locals params stmts) access

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

    -- | Functional n-ary operation expression: op(a1,a2,..,an).
    fOp :: ()
        => ([Basic] -> m Basic)     -- ^ n-ary Operation op.
        -> [CoreLambda m]           -- ^ Operand expressions a1,a2,..,an.
        -> CoreLambda m             -- ^ n-ary operation.
    fOp = FOp 

    -- | Functional function call: f(a1,a2,..,an).
    fCall :: ()
        => Ident            -- ^ Identifier f.
        -> [CoreLambda m]   -- ^ Argument expressions a1,a2,..,an.
        -> CoreLambda m     -- ^ Functional function call.
    fCall = FCall 

    -- | Letrec expression: let i1 := e1; i2 := e2; .. ; in := en; in e. 
    fLet :: () 
        => [(Ident, CoreLambda m)]  -- ^ Identifier ii and expression ei.
        -> CoreLambda m             -- ^ Expression e.
        -> CoreLambda m             -- ^ Letrec expression.
    fLet = FLet

    -- | Functional application expression: f a1 a2 .. an
    fApp :: ()
        => CoreLambda m     -- ^ Function expression f.
        -> [CoreLambda m]   -- ^ Argument expressions a1, a2, .. , an.
        -> CoreLambda m     -- ^ Functional application expression.
    fApp = FApp 

    -- | Functional abstraction expression: λp1 p2 .. pn . e
    fAbs :: ()
        => [Ident]      -- ^ Parameters p1, p2, .., pn.
        -> CoreLambda m -- ^ Expression e.
        -> CoreLambda m -- ^ Functional abstraction expression.
    fAbs = FAbs 

    -- | New logical query.
    query :: ()
        => [Ident]      -- ^ Parameters.
        -> Maybe Ident  -- ^ Maybe the variable, for which a value is sought; if Nothing: boolean is returned.
        -> [Ident]      -- ^ Knowledge bases to be asked.
        -> [CoreClause] -- ^ Goals of the query.
        -> Subprogram m -- ^ Logical query.
    query params sought bases clause = Logical $ Query params sought bases clause 

    -- | Logical disjunction of goals: g1;g2;..;gn.
    lOr :: ()
        => [[CoreClause]]   -- ^ List of konjoined goals g1,g2,..,gn.
        -> CoreClause       -- ^ Logical disjunction of goals.
    lOr = LOr  

    -- | Negation of goals: ¬g.
    lNot :: ()
        => [CoreClause] -- ^ list of goals g.
        -> CoreClause   -- ^ Negation of goals.
    lNot = LNot 

    -- | A logical goal: g.
    lGoal :: ()
        => CoreGoal     -- ^ Goal g.
        -> CoreClause   -- ^ Logical goal.
    lGoal = LGoal 

    -- | A n-ary predicate goal: p(t1,t2,..,tn).
    lPred :: ()
        => Ident        -- ^ Identifier p. 
        -> [CoreTerm]   -- ^ Argument terms t1,t2,..,tn.
        -> CoreGoal     -- ^ n-ary predicate.
    lPred ident args = LPred ident (length args) args

    -- | A unify goal: t1 := t2.
    lUnify :: ()
        => CoreTerm     -- ^ Term t1.
        -> CoreTerm     -- ^ Term t2.
        -> CoreGoal     -- ^ Unify goal.
    lUnify = (:=:)

    -- | A constant atom term: a.
    lAtom :: () 
        => Basic        -- ^ Basic value a.
        -> CoreTerm     -- ^ A constant atom.
    lAtom = Atom 

    -- | A logical function term: f(t1,t2,..,tn).
    lFun :: ()
        => Ident        -- ^ Identifier f.
        -> [CoreTerm]   -- ^ Argument terms t1, t2, .., tn.
        -> CoreTerm     -- ^ Logical function term.
    lFun ident args = LFun ident (length args) args

    -- | A knowledge base: r1 r2 .. rn.
    base :: ()
        => [CoreRule]       -- ^ Rules r1, r2, .., rn.
        -> Subprogram m     -- ^ Subprogram representing the knowledge base.
    base = Base

    -- | A logical rule: h :- b..
    lRule :: ()
        => CoreGoal         -- ^ Head predicate h.
        -> [CoreClause]     -- ^ Body goal b.
        -> CoreRule         -- ^ Logical rule.
    lRule = (:-)

    -- | A basic NULL value: NULL.
    bNull :: ()
        => Basic    -- ^ NULL.
    bNull = Null

    -- | A basic boolean value: b.
    bBool :: ()
        => Bool     -- ^ Boolean value b.
        -> Basic    -- ^ Basic boolean value.
    bBool = Boolean

    -- | A basic integer value: i.
    bNat :: ()
        => Integer  -- ^ Integer value i.
        -> Basic    -- ^ Basic integer value.
    bNat = Natural

    -- | A basic double value: d.
    bReal :: ()
        => Double   -- ^ Double value d.
        -> Basic    -- ^ Basic double value.
    bReal = Real 

    -- | A basic struct value: s(b1,b2,..,bn).
    bStruct :: ()
        => Ident    -- ^ Identifier s.
        -> [Basic]  -- ^ Argument basic values b1,b2,..,bn.
        -> Basic    -- ^ Basic struct value.
    bStruct name bscs = Struct name (length bscs) bscs