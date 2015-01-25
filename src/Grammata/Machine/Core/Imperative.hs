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
-- | Module : Grammata.Machine.Core.Imperative
-- Description : Grammata Imperative Core Language 
-- Maintainer : sascha.rechenberger@uni-ulm.de
-- Stability : stable
-- Portability : portable
-- Copyright : (c) Sascha Rechenberger, 2014
-- License : GPL-3
---------------------------------------------------------------------------

module Grammata.Machine.Core.Imperative
(

    -- * Imperative core language evaluation monad
    CoreImperative (..),

    -- * ASTs
    -- ** Imperative Statements
    CoreStatement (..),
    -- ** Arithmetical Expressions
    CoreExpression (..), evalExpressionlist,
    
    -- * Imperative Method
    runImperativeSubprogram
)
where 

    import Debug.Trace

    import Prelude hiding (toInteger)
    
    import Grammata.Machine.Core.Class (CoreGeneral (..), Ident, Pointer)
    import Grammata.Machine.Core.Types (Basic (..), toBoolean, toInteger)

    import Control.Applicative ((<|>), pure, (<*>), (<$>))
    import Control.Monad (forM)


    -- | A imperative method (function or procedure) represented by local variables, parameters and it's code.
    -- data CoreMethod m = Method [(Ident, CoreExpression m)] [Ident] [CoreStatement m] 


    -- | Imperative core language AST.
    data CoreStatement m = 
        -- | <IDENT> := <EXPR>
          Ident := CoreExpression m
        -- | if <EXPR> then <STMT>* else <STMT>*
        | IIf (CoreExpression m) [CoreStatement m] [CoreStatement m]
        -- | while <EXPR> do <STMT>* 
        | IWhile (CoreExpression m) [CoreStatement m]
        -- | return <EXPR>
        | IReturn (CoreExpression m)
        -- | call <IDENT> (<EXPR>*)
        | ICall Ident [CoreExpression m]
        -- | trackback
        | IBackTrack
        -- | keep <EXPR>
        | IKeep (CoreExpression m)



    -- | Arithmetical Expression AST.
    data CoreExpression m = 
        -- | <IDENT>
          IVar Ident
        -- | <BASIC>
        | IVal Basic
        -- | <OP> <BASIC>*
        | IOp ([Basic] -> m Basic) [CoreExpression m]
        -- | <IDENT> (<EXPR>*)
        | IFunc Ident [CoreExpression m]
        -- | remind
        | IRemind

    instance Show (CoreStatement m) where
        show (_ := _) = "assingment"
        show (IIf _ c1 c2) = "if"
        show (IWhile _ c) = "while"
        show (IReturn _) = "return"
        show (ICall _ _) = "call" 
        show (IBackTrack) = "trackback"
        show (IKeep _) = "keep"

        
    -- | Imperative core language evaluation monad class.
    class CoreGeneral m => CoreImperative m where
        -- | Writing to the stack, either locally or or generally.
        writeSymbol :: Ident -> Basic -> m ()


    -- | Runs a given core method given arguments and a return point.
    runImperativeSubprogram :: (CoreImperative m) 
        => [(Ident, CoreExpression m)] 
        -> [Ident] 
        -> [CoreStatement m]     -- ^ Method to execute.
        -> [Basic]          -- ^ Arguments of the method call.
        -> (Basic -> m ())  -- ^ Returning point.
        -> m ()             -- ^ Remaining program action.
    runImperativeSubprogram locals params stmts args retPt = let p_as = params `zip` args in do
        enter p_as
        let (names, exprs) = unzip locals 
        evalExpressionlist locals exprs [] $ \vals -> let locals' = names `zip` vals in do 
            leave
            enter (locals' ++ p_as)
            runImperative stmts $ \bsc -> do
                leave
                retPt bsc


    -- | Evaluates a arithmetical expression and applies the function of its returning point to the evaluated value.
    evalExpression :: (CoreImperative m)
        => [(Ident, CoreExpression m)]  -- ^ Temporary auxiliary symbol table.
        -> (Basic -> m ())              -- ^ Return point function.
        -> CoreExpression m             -- ^ Expression to evaluate.
        -> m ()                         -- ^ Evaluation action.
    evalExpression tmp retPt expr = case expr of
        IVar var        -> (readTemp tmp var >>= evalExpression tmp retPt) <|> (readSymbol var >>= retPt)
        IVal val        -> retPt val 
        IOp f args      -> evalExpressionlist tmp args [] $ (\bscs -> f bscs >>= retPt)
        IFunc name args -> evalExpressionlist tmp args [] $ (call name retPt)
        IRemind         -> remind >>= retPt

        where 
            readTemp tmp var = case var `lookup` tmp of
                Nothing -> fail $ "ERROR CORE.IMPERATIVE cannot find identifier " ++ var ++ " in " ++ show (fst . unzip $ tmp) ++ "."
                Just x  -> return x

    -- | Evaluates a list of expressions and applies the function of its returning point to the evalated values.
    evalExpressionlist :: (CoreImperative m)
        => [(Ident, CoreExpression m)]  -- ^ Temporary auxiliary symbol table.
        -> [CoreExpression m]           -- ^ Expressions to evaluate.
        -> [Basic]                      -- ^ Accumulator for evaluated expressions.
        -> ([Basic] -> m ())            -- ^ Return point function.
        -> m ()                         -- ^ Evaluation action.
    evalExpressionlist tmp exprs evaluated retPt = case exprs of 
        []   -> retPt . reverse $ evaluated
        e:es -> evalExpression tmp (\bsc -> evalExpressionlist tmp es (bsc:evaluated) retPt) e 


    -- | Executes the given sequence of 'CoreStatement's.
    runImperative :: (CoreImperative m) 
        => [CoreStatement m] -- ^ Sequence of 'CoreStatement's.
        -> (Basic -> m ())   -- ^ Return point.
        -> m ()              -- ^ Execution action.
    runImperative []        _     = fail "ERROR CORE.IMPERATIVE unexpected end of program."
    runImperative (s:stmts) retPt = case s of 
        ident := expr -> 
            evalExpression [] (\bsc -> writeSymbol ident bsc >> runImperative stmts retPt) expr 
        IIf cond thenBlock elseBlock -> 
            evalExpression [] (\bsc -> toBoolean bsc >>= \cond -> runImperative ((if cond then thenBlock else elseBlock) ++ stmts) retPt) cond
        IWhile cond block ->
            evalExpression [] (\bsc -> toBoolean bsc >>= \cond -> runImperative ((if cond then block ++ [s] else []) ++ stmts) retPt) cond
        IReturn expr -> 
            evalExpression [] retPt expr
        ICall name exprs -> 
            evalExpressionlist [] exprs [] $ call name $ \_ -> 
                runImperative stmts retPt
        IBackTrack -> 
            trackback
        IKeep expr -> 
            evalExpression [] (\bsc -> keep bsc >> runImperative stmts retPt) expr


{-
    -- | Evaluates an arithmetical expression.
    evalExpression :: (CoreImperative m)
        => [(Ident, CoreExpression m)]  -- ^ Temporary auxiliary symbol table.
        -> CoreExpression m             -- ^ Expression to evaluate.
        -> m ()                         -- ^ List of possible results.
    evalExpression tmp (IVar id) = (readTemp tmp >>= evalExpression tmp) <|> (readSymbol id >>= setReturnValue)
        where 
            readTemp tmp = case id `lookup` tmp of
                Nothing -> fail $ "Cannot find " ++ id ++ " in TMP."
                Just e  -> return e
    evalExpression _ (IVal bsc) = setReturnValue bsc
    evalExpression tmp (IOp f args) = f <$> mapM (\arg -> evalExpression tmp arg >> getReturnValue) args >>= setReturnValue
    evalExpression tmp (IFunc name args) = mapM (\arg -> evalExpression tmp arg >> getReturnValue) args >>= callFunction name 

    -- | Evaluates a expression and extracts a boolean.
    evalToBoolean :: CoreImperative m 
        => CoreExpression m -- ^ Expression to evaluate.
        -> m Bool           -- ^ Result.
    evalToBoolean cond = do
        evalExpression [] cond 
        cond <- getReturnValue
        case cond of
            Boolean b -> return b
            others    -> fail $ "ERROR CORE.IMPERATIVE " ++ show others ++ " is no boolean."

    -- | Evaluates a expression and extracts an integer.
    evalToInteger :: CoreImperative m 
        => CoreExpression m -- ^ Expression to evaluate.
        -> m Integer    -- ^ Result.
    evalToInteger val = do
        evalExpression [] val 
        val <- getReturnValue
        case val of
            Natural b -> return b
            others    -> fail $ "ERROR CORE.IMPERATIVE " ++ show others ++ " is no natural."

    -- | Runs a method as a function taking its arguments and returing a result.
    runFunction :: CoreImperative m 
        => CoreMethod m     -- ^ Method to run.
        -> [Basic]      -- ^ Arguments.
        -> m Basic      -- ^ Result.
    runFunction (Method locals params code) args = let pLocals = params `zip` args in do 
        enter pLocals
        locals' <- mapM (\(i,e) -> evalExpression locals e >>= mapM (return . (,) i)) locals >>= return . parallel -- >>= mapM (\es -> (fst . unzip $ locals) `zip` es)
        leave 
        choice . flip map locals' $ \loc -> do
            enter $ pLocals ++ loc
            toReturn <- run code
            leave
            return toReturn
        where 
            run [] = fail "ERROR CORE.IMPERATIVE no return"
            run (stmt:stmts) = case stmt of
                ident := expr  -> evalExpression [] expr >>= choice . map (\basic -> writeSymbol ident basic >> run stmts)
                IIf cond b1 b2 -> do
                    cond' <- evalToBoolean cond
                    if cond' then run (b1 ++ stmts) else run (b2 ++ stmts)
                IWhile cond block -> do 
                    cond' <- evalToBoolean cond 
                    if cond' then run $ block ++ stmt:stmts else run stmts
                IReturn expr -> evalExpression [] expr >>= choice . map return
                ICall ident exprs -> mapM (evalExpression []) exprs >>= return . parallel >>= choice . map (\args -> call ident args >> run stmts)
                BackTrack -> trackBack 

    -- | Runs a method as a procedure taking its arguments.
    runProcedure :: CoreImperative m 
        => CoreMethod m     -- ^ Method to run.
        -> [Basic]      -- ^ Arguments.
        -> m ()         -- ^ Void.
    runProcedure (Method locals params code) args = let pLocals = params `zip` args in do 
        enter pLocals 
        locals' <- mapM (\(i,e) -> evalExpression locals e >>= mapM (return . (,) i)) locals >>= return . parallel
        leave
        choice . flip map locals' $ \loc -> do 
            enter $ pLocals ++ loc 
            run code 
            leave 
        where
            run [] = return ()
            run (stmt:stmts) = case stmt of
                ident := expr  -> evalExpression [] expr >>= choice . map (\basic -> writeLocals ident basic >> run stmts)
                IIf cond b1 b2 -> do
                    cond' <- evalToBoolean cond
                    if cond' then run (b1 ++ stmts) else run (b2 ++ stmts)
                IWhile cond block -> do 
                    cond' <- evalToBoolean cond 
                    if cond' then run $ block ++ stmt:stmts else run stmts
                IReturn e -> do
                    n <- evalToInteger e 
                    case n of 
                        0 -> return ()
                        _ -> fail $ "ERROR CORE.IMPERATIVE exit code " ++ show n ++ "."
                ICall ident exprs -> mapM (evalExpression []) exprs >>= return . parallel >>= choice . map (\args -> call ident args >> run stmts)
                BackTrack -> trackBack 

-}