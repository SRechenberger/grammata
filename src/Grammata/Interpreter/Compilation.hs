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
    compileProgram, Program (..)
)
where

    import Debug.Trace

    import Grammata.Machine (
        imperative, functional, query, base, Subprogram,
        iAssignment, iIf, iWhile, iReturn, iCall, iTrackBack, iVal, iVar, iOp, iFunc, iKeep, iRemind, CoreStatement, CoreExpression,
        fVar, fConst, fIf, fOp, fCall, fLet, fApp, fAbs, fKeep, fBackTrack, fRemind, CoreLambda,
        lOr, lNot, lGoal, lUnify, lFun, lAtom, lRule, lPred, lVar, CoreClause, CoreRule, CoreGoal, CoreTerm,
        bBool, bNat, bReal, bStruct, bNull, Basic, (=:=), (=/=),
        Machine, Ident
        )
    import qualified Grammata.Machine as M (Basic (Natural, Boolean, Real, Struct))
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
        Goal (..),
        Term (..)
        )

    import Grammata.Interpreter.Compiler (Compiler, runCompiler, getVars, getFuncs)

    import Control.Applicative ((<$>), (<*>), pure)

    import Control.Monad (forM)

    type COMPILER = Compiler [Ident] [Ident]

    compileProgram :: ()
        => Program 
        -> COMPILER ([(Ident, Subprogram Machine)],[(Ident, CoreExpression Machine)])
    compileProgram prg = do
        globals' <- compileGlobals $ globals prg 
        subprgs' <- compileSubprograms $ subs prg 
        return (subprgs', globals')

 
    compileSubprograms :: ()
        => [(String, Subprg)]
        -> COMPILER [(Ident, Subprogram Machine)]
    compileSubprograms sprgs = do
        sprgs' <- forM sprgs $ \(ident, sprg) -> do 
            sprg' <- compileSubprogram sprg 
            return (ident, sprg')
        return sprgs'


    compileSubprogram :: ()
        => Subprg
        -> COMPILER (Subprogram Machine)
    compileSubprogram (Procedure params locals stmts) = do 
        locals' <- forM locals $ \(ident, expr) -> case expr of
            Nothing -> return (ident, iVal bNull)
            Just e  -> do 
                e' <- compileImperativeExpression e 
                return (ident, e')
        stmts' <- concat <$> mapM compileImperativeStatement stmts
        return $ imperative locals' params stmts' 
    compileSubprogram (Lambda params lambda) = do
        lambda' <- compileLambda lambda 
        return $ functional lambda' params
    compileSubprogram (Query params bases sought clause) = do 
        clause' <- compileClause clause 
        return $ query params sought bases clause'
    compileSubprogram (Base rules) = do 
        rules' <- mapM compileRule rules
        return $ base rules'


    compileRule :: ()
        => Rule 
        -> COMPILER CoreRule
    compileRule (head :- body) = do
        head' <- compileGoal head 
        body' <- case body of 
            Nothing -> return []
            Just cl -> compileClause cl 
        return $ lRule head' body'


    compileGoal :: ()
        => Goal 
        -> COMPILER CoreGoal
    compileGoal (t1 :=: t2) = do 
        t1' <- compileTerm t1
        t2' <- compileTerm t2 
        return $ lUnify t1' t2'
    compileGoal (Predicate name terms) = do 
        terms' <- mapM compileTerm terms
        return $ lPred name terms'


    compileTerm :: ()
        => Term 
        -> COMPILER CoreTerm
    compileTerm (Val val) = do 
        val' <- compileBasicValue val 
        return $ lAtom val'
    compileTerm (Var var) = return $ lVar var 
    compileTerm (Expr expr) = compileLogicalExpression expr 


    compileLogicalExpression :: ()
        => Expression Term 
        -> COMPILER CoreTerm
    compileLogicalExpression (Const term) = compileTerm term
    compileLogicalExpression (BinOp e1 op e2) = lFun op <$> mapM compileLogicalExpression [e1,e2]
    compileLogicalExpression (UnOp op e) = lFun op <$> mapM compileLogicalExpression [e]
    compileLogicalExpression (Func name es) = lFun name <$> mapM compileLogicalExpression es
    compileLogicalExpression Remind = fail $ "ERROR remind is not allowed in logical subprograms."


    compileClause :: ()
        => Clause 
        -> COMPILER [CoreClause]
    compileClause (Pos goal) = (:[]) . lGoal <$> compileGoal goal 
    compileClause (Neg clause) = (:[]) . lNot <$> compileClause clause 
    compileClause (c1 :&& c2) = (++) <$> compileClause c1 <*> compileClause c2
    compileClause (c1 :|| c2) = (:[]) . lOr <$> mapM compileClause [c1, c2]

   
    compileLambda :: ()
        => Lambda 
        -> COMPILER (CoreLambda Machine)
    compileLambda (Fun_Symbol sym) = fVar <$> pure sym 
    compileLambda (Fun_Value val) = fConst <$> compileBasicValue val 
    compileLambda (Fun_Cond c a b) = fIf <$> compileLambda c <*> compileLambda a <*> compileLambda b 
    compileLambda (Fun_Abstr ids e) = fAbs ids <$> compileLambda e 
    compileLambda (Fun_Appl f as) = fApp <$> compileLambda f <*> mapM compileLambda as 
    compileLambda (Fun_Let defs e) = do 
        defs' <- forM defs $ \(id, def) -> do 
            def' <- compileLambda def 
            return (id, def')
        e' <- compileLambda e 
        return (fLet defs' e')
    compileLambda (Fun_Arith expr) = compileFunctionalExpression expr 
    compileLambda (Fun_Keep expr) = fKeep <$> compileLambda expr
--    compileLambda (Fun_Remind) = pure fRemind
    compileLambda (Fun_Backtrack) = pure fBackTrack


    compileFunctionalExpression :: ()
        => Expression Lambda 
        -> COMPILER (CoreLambda Machine) 
    compileFunctionalExpression (Const lambda) = compileLambda lambda 
    compileFunctionalExpression (BinOp l1 op l2) = fOp <$> compileBinaryOperator op <*> mapM compileFunctionalExpression [l1,l2]
    compileFunctionalExpression (UnOp op l) = fOp <$> compileUnaryOperator op <*> mapM compileFunctionalExpression [l]
    compileFunctionalExpression (Func name as) = fCall name <$> mapM compileFunctionalExpression as
    compileFunctionalExpression Remind = pure fRemind


    compileImperativeStatement :: ()
        => Statement 
        -> COMPILER [CoreStatement Machine]
    compileImperativeStatement (Imp_Assign ident expr) = do 
        expr' <- compileImperativeExpression expr 
        return [iAssignment ident expr']
        
    compileImperativeStatement (Imp_For cnt init limit step stmts) = do 
        init' <- case init of
            Nothing -> return []
            Just e  -> compileImperativeStatement (Imp_Assign cnt e)
        stmts' <- concat <$> mapM compileImperativeStatement stmts
        step' <- let 
            stepWidth = case step of 
                Nothing -> Const $ Natural 1
                Just s  -> s
            in compileImperativeStatement (Imp_Assign cnt (BinOp (Const $ Variable cnt) "+" stepWidth))
        cond <- compileImperativeExpression (BinOp (Const $ Variable cnt) "<=" limit)
        return $ init' ++ [iWhile cond (stmts' ++ step')] 
        
    compileImperativeStatement (Imp_DoWhile stmts cond) = do 
        stmts' <- concat <$> mapM compileImperativeStatement stmts
        cond' <- compileImperativeExpression cond 
        return $ stmts' ++ [iWhile cond' stmts']
        
    compileImperativeStatement (Imp_While cond stmts) = do
        cond' <- compileImperativeExpression cond 
        stmts' <- concat <$> mapM compileImperativeStatement stmts
        return [iWhile cond' stmts']
        
    compileImperativeStatement (Imp_If cond thenBlock elseBlock) = do 
        cond' <- compileImperativeExpression cond 
        thenBlock' <- concat <$> mapM compileImperativeStatement thenBlock
        elseBlock' <- concat <$> mapM compileImperativeStatement elseBlock
        return [iIf cond' thenBlock' elseBlock']
        
    compileImperativeStatement (Imp_Call ident exprs) = do 
        exprs' <- mapM compileImperativeExpression exprs 
        return [iCall ident exprs'] 
        
    compileImperativeStatement (Imp_Return expr) = do 
        expr' <- compileImperativeExpression expr 
        return [iReturn expr']
    
    compileImperativeStatement Imp_Backtrack = return [iTrackBack]

    compileImperativeStatement (Imp_Keep e) = (:[]) <$> (iKeep <$> compileImperativeExpression e)


    
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
    compileImperativeExpression (Const (Variable v)) = return $ iVar v
    compileImperativeExpression (Const val) = iVal <$> compileBasicValue val 
    compileImperativeExpression (BinOp e1 op e2) = do
        e1' <- compileImperativeExpression e1
        op' <- compileBinaryOperator op 
        e2' <- compileImperativeExpression e2
        return $ iOp op' [e1', e2']
    compileImperativeExpression (UnOp op e) = do 
        op' <- compileUnaryOperator op 
        e'  <- compileImperativeExpression e 
        return $ iOp op' [e']
    compileImperativeExpression (Func id es) = iFunc id <$> mapM compileImperativeExpression es 
    compileImperativeExpression Remind = return iRemind


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
        others                 -> fail $ "ERROR " ++ show others ++ " are no valid operands for '<'."
    compileBinaryOperator "<=" = return $ \ops -> case ops of 
        [M.Natural a, M.Natural b] -> return . M.Boolean $ a <= b 
        [M.Real a, M.Real b]       -> return . M.Boolean $ a <= b
        others                 -> fail $ "ERROR " ++ show others ++ " are no valid operands for '<='."
    compileBinaryOperator ">=" = return $ \ops -> case ops of 
        [M.Natural a, M.Natural b] -> return . M.Boolean $ a >= b 
        [M.Real a, M.Real b]       -> return . M.Boolean $ a >= b
        others                 -> fail $ "ERROR " ++ show others ++ " are no valid operands for '>='."  
    compileBinaryOperator ">" = return $ \ops -> case ops of 
        [M.Natural a, M.Natural b] -> return . M.Boolean $ a > b 
        [M.Real a, M.Real b]       -> return . M.Boolean $ a > b
        others                 -> fail $ "ERROR " ++ show others ++ " are no valid operands for '>'."   
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
    compileBinaryOperator ":" = return $ \ops -> case ops of
        [a,tail@(M.Struct "cons" 2 as)] -> return $ M.Struct "cons" 2 [a,tail]
        [a,tail@(M.Struct "nil" 0 [])] -> return $ M.Struct "cons" 2 [a,tail]
        others -> fail $ "ERROR " ++ show others ++ " cannot be a list."



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
    compileUnaryOperator "." = return $ \ops -> case ops of
        [M.Struct "cons" 2 [a,t]] -> return a 
        others        -> fail $ "ERROR " ++ show others ++ " has no head."
    compileUnaryOperator "%" = return $ \ops -> case ops of
        [M.Struct "cons" 2 [_,t]] -> return t 
        others        -> fail $ "ERROR " ++ show others ++ " has no tail."


    compileBasicValue :: ()
        => Value 
        -> COMPILER Basic 
    compileBasicValue (Natural n) = return $ bNat n 
    compileBasicValue (Real r) = return $ bReal r 
    compileBasicValue (Boolean b) = return $ bBool b
    compileBasicValue (List xs) = foldr cons nil <$> (mapM compileBasicValue xs)
        where
            cons x xs = bStruct "cons" [x,xs]
            nil = bStruct "nil" []