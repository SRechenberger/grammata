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
        imperative, functional, query, base, Subprogram,
        iAssignment, iIf, iWhile, iReturn, iCall, iTrackBack, iVal, iVar, iOp, iFunc, CoreStatement, CoreExpression,
        fVar, fConst, fIf, fOp, fCall, fLet, fApp, fAbs,
        lOr, lNot, lGoal, lUnify, lFun, lAtom, lRule, lPred, lVar,
        bBool, bNat, bReal, bStruct, bNull, Basic, (=:=), (=/=),
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

    import Grammata.Interpreter.Compiler (Compiler, runCompiler, getVars, getFuncs)

    import Control.Applicative ((<$>), (<*>))

    import Control.Monad (forM)

    type COMPILER = Compiler [Ident] [Ident]

    compileProgram :: ()
        => Program 
        -> COMPILER ([(Ident, Subprogram Machine)],[(Ident, CoreExpression Machine)])
    compileProgram prg = do
        globals' <- compileGlobals $ globals prg 
        subprgs' <- compileSubprograms $ subs prg 
        return (subprgs', globals')

    -- *** TODO **************************************************
    compileSubprograms :: ()
        => [(String, Subprg)]
        -> COMPILER [(Ident, Subprogram Machine)]
    compileSubprograms = undefined
    -- ***********************************************************
    
    compileGlobals :: ()
        => [(String, Maybe (Expression Value))]
        -> COMPILER [(Ident, CoreExpression Machine)]
    compileGlobals globs = do 
        globals' <- forM globs $ \(ident, expr) -> do
            expr' <- case expr of 
                Nothing -> return $ iVal bNull
                Just e  -> compileImperativeExpression e 
            return (ident, expr')
        return globals'

    compileImperativeExpression :: ()
        => Expression Value
        -> COMPILER (CoreExpression Machine)
    compileImperativeExpression (Const val)      = iVal <$> compileBasicValue val 
    compileImperativeExpression (BinOp e1 op e2) = do
        e1' <- compileImperativeExpression e1
        op' <- compileBinaryOperator op 
        e2' <- compileImperativeExpression e1
        return $ iOp op' [e1', e2']
    compileImperativeExpression (UnOp op e)      = do 
        op' <- compileUnaryOperator op 
        e'  <- compileImperativeExpression e 
        return $ iOp op' [e']
    compileImperativeExpression (Func id es)     = iFunc id <$> mapM compileImperativeExpression es 


    compileBinaryOperator :: ()
        => String 
        -> COMPILER ([Basic] -> Machine Basic)
    compileBinaryOperator "+" = return $ \ops -> case ops of 
        [M.Natural a, M.Natural b] -> return . M.Natural $ a + b 
        [M.Real a, M.Real b]       -> return . M.Real $ a + b
        others                 -> fail $ "ERROR " ++ show others ++ " are no valid operands for '+'."
    compileBinaryOperator "-" = return $ \ops -> case ops of 
        [M.Natural a, M.Natural b] -> return . M.Natural $ a - b 
        [M.Real a, M.Real b]       -> return . M.Real $ a - b
        others                 -> fail $ "ERROR " ++ show others ++ " are no valid operands for '-'."
    compileBinaryOperator "*" = return $ \ops -> case ops of 
        [M.Natural a, M.Natural b] -> return . M.Natural $ a * b 
        [M.Real a, M.Real b]       -> return . M.Real $ a * b
        others                 -> fail $ "ERROR " ++ show others ++ " are no valid operands for '*'."
    compileBinaryOperator "/" = return $ \ops -> case ops of 
        [M.Natural a, M.Natural b] -> return . M.Natural $ a `div` b 
        [M.Real a, M.Real b]       -> return . M.Real $ a / b
        others                 -> fail $ "ERROR " ++ show others ++ " are no valid operands for '/'."    
    compileBinaryOperator "<" = return $ \ops -> case ops of 
        [M.Natural a, M.Natural b] -> return . M.Boolean $ a < b 
        [M.Real a, M.Real b]       -> return . M.Boolean $ a < b
        others                 -> fail $ "ERROR " ++ show others ++ " are no valid operands for '+'."
    compileBinaryOperator "<=" = return $ \ops -> case ops of 
        [M.Natural a, M.Natural b] -> return . M.Boolean $ a <= b 
        [M.Real a, M.Real b]       -> return . M.Boolean $ a <= b
        others                 -> fail $ "ERROR " ++ show others ++ " are no valid operands for '-'."
    compileBinaryOperator ">=" = return $ \ops -> case ops of 
        [M.Natural a, M.Natural b] -> return . M.Boolean $ a >= b 
        [M.Real a, M.Real b]       -> return . M.Boolean $ a >= b
        others                 -> fail $ "ERROR " ++ show others ++ " are no valid operands for '/'."  
    compileBinaryOperator ">" = return $ \ops -> case ops of 
        [M.Natural a, M.Natural b] -> return . M.Boolean $ a > b 
        [M.Real a, M.Real b]       -> return . M.Boolean $ a > b
        others                 -> fail $ "ERROR " ++ show others ++ " are no valid operands for '/'."   
    compileBinaryOperator "||" = return $ \ops -> case ops of 
        [M.Boolean a, M.Boolean b] -> return . M.Boolean $ a || b 
        others                 -> fail $ "ERROR " ++ show others ++ " are no valid operands for '||'." 
    compileBinaryOperator "&&" = return $ \ops -> case ops of 
        [M.Boolean a, M.Boolean b] -> return . M.Boolean $ a && b 
        others                 -> fail $ "ERROR " ++ show others ++ " are no valid operands for '&&'."
    compileBinaryOperator "!=" = return $ \ops -> case ops of 
        [a,b]  -> M.Boolean <$> a =/= b 
        others -> fail $ "ERROR " ++ show others ++ " are no valid operands for '!='." 
    compileBinaryOperator "==" = return $ \ops -> case ops of 
        [a,b]  -> M.Boolean <$> a =:= b 
        others -> fail $ "ERROR " ++ show others ++ " are no valid operands for '=='." 


    compileUnaryOperator :: ()
        => String 
        -> COMPILER ([Basic] -> Machine Basic)
    compileUnaryOperator "!" = return $ \ops -> case ops of 
        [M.Boolean a] -> return . M.Boolean . not $ a 
        others      -> fail $ "ERROR " ++ show others ++ " are no valid operands for '!'."
    compileUnaryOperator "-" = return $ \ops -> case ops of 
        [M.Natural a] -> return . M.Natural . negate $ a 
        [M.Real a]    -> return . M.Real . negate $ a 
        others      -> fail $ "ERROR " ++ show others ++ " are no valid operands for '-'."

    compileBasicValue :: ()
        => Value 
        -> COMPILER Basic 
    compileBasicValue (Natural n) = return $ bNat n 
    compileBasicValue (Real r) = return $ bReal r 
    compileBasicValue (Boolean b) = return $ bBool b