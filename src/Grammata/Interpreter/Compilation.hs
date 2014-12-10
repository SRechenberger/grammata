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
        iAssignment, iIf, iWhile, iReturn, iCall, iTrackBack, iVal, iVar, iOp, iFunc, CoreStatement, CoreExpression,
        fVar, fConst, fIf, fOp, fCall, fLet, fApp, fAbs, CoreLambda,
        lOr, lNot, lGoal, lUnify, lFun, lAtom, lRule, lPred, lVar, CoreClause, CoreRule, CoreGoal, CoreTerm,
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
    compileSubprogram (Procedure ret params locals stmts) = do 
        locals' <- forM locals $ \(ident, expr) -> case expr of
            Nothing -> return (ident, iVal bNull)
            Just e  -> do 
                e' <- compileImperativeExpression e 
                return (ident, e')
        stmts' <- concat <$> mapM compileImperativeStatement stmts
        return $ imperative locals' params stmts' (ret == Something) 
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
    compileLambda (Symbol sym) = fVar <$> pure sym 
    compileLambda (Value val) = fConst <$> compileBasicValue val 
    compileLambda (Cond c a b) = fIf <$> compileLambda c <*> compileLambda a <*> compileLambda b 
    compileLambda (Abstr ids e) = fAbs ids <$> compileLambda e 
    compileLambda (Appl f as) = fApp <$> compileLambda f <*> mapM compileLambda as 
    compileLambda (Let defs e) = do 
        defs' <- forM defs $ \(id, def) -> do 
            def' <- compileLambda def 
            return (id, def')
        e' <- compileLambda e 
        return (fLet defs' e')
    compileLambda (Arith expr) = compileFunctionalExpression expr 


    compileFunctionalExpression :: ()
        => Expression Lambda 
        -> COMPILER (CoreLambda Machine) 
    compileFunctionalExpression (Const lambda) = compileLambda lambda 
    compileFunctionalExpression (BinOp l1 op l2) = fOp <$> compileBinaryOperator op <*> mapM compileFunctionalExpression [l1,l2]
    compileFunctionalExpression (UnOp op l) = fOp <$> compileUnaryOperator op <*> mapM compileFunctionalExpression [l]
    compileFunctionalExpression (Func name as) = fCall name <$> mapM compileFunctionalExpression as


    compileImperativeStatement :: ()
        => Statement 
        -> COMPILER [CoreStatement Machine]
    compileImperativeStatement (ident := expr) = do 
        expr' <- compileImperativeExpression expr 
        return [iAssignment ident expr']
        
    compileImperativeStatement (For cnt init limit step stmts) = do 
        init' <- case init of
            Nothing -> return []
            Just e  -> compileImperativeStatement (cnt := e)
        stmts' <- concat <$> mapM compileImperativeStatement stmts
        step' <- let 
            stepWidth = case step of 
                Nothing -> Const $ Natural 1
                Just s  -> s
            in compileImperativeStatement (cnt := BinOp (Const $ Variable cnt) "+" stepWidth)
        cond <- compileImperativeExpression (BinOp (Const $ Variable cnt) "<=" limit)
        return $ init' ++ [iWhile cond (stmts' ++ step')] 
        
    compileImperativeStatement (DoWhile stmts cond) = do 
        stmts' <- concat <$> mapM compileImperativeStatement stmts
        cond' <- compileImperativeExpression cond 
        return $ stmts' ++ [iWhile cond' stmts']
        
    compileImperativeStatement (While cond stmts) = do
        cond' <- compileImperativeExpression cond 
        stmts' <- concat <$> mapM compileImperativeStatement stmts
        return [iWhile cond' stmts']
        
    compileImperativeStatement (If cond thenBlock elseBlock) = do 
        cond' <- compileImperativeExpression cond 
        thenBlock' <- concat <$> mapM compileImperativeStatement thenBlock
        elseBlock' <- concat <$> mapM compileImperativeStatement elseBlock
        return [iIf cond' thenBlock' elseBlock']
        
    compileImperativeStatement (Call ident exprs) = do 
        exprs' <- mapM compileImperativeExpression exprs 
        return [iCall ident exprs'] 
        
    compileImperativeStatement (Return expr) = do 
        expr' <- compileImperativeExpression expr 
        return [iReturn expr']
        
    compileImperativeStatement Exit = return [iReturn (iVal bNull)]
    
    compileImperativeStatement Backtrack = return [iTrackBack]


    
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
        others                 -> fail $ "ERROR " ++ show others ++ " are no valid operands for '<='."
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