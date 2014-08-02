{-|
Module : Grammata.Machine.Core.Imperative
Description : Grammata Imperative Core Language 
Maintainer : sascha.rechenberger@uni-ulm.de
Stability : stable
Portability : portable
Copyright : (c) Sascha Rechenberger, 2014
License : GPL-3

This file is part of grammata.

grammata is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

grammata is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with grammata. If not, see <http://www.gnu.org/licenses/>.
-}

-- {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Grammata.Machine.Core.Imperative
(

    -- * Imperative core language evaluation monad
    CoreImperative (..),

    -- * ASTs
    -- ** Imperative Statements
    CoreStatement (..),
    -- ** Arithmetical Expressions
    Expression (..),
    
    -- * Imperative Method
    Method (..),    
    runProcedure,
    runFunction
)
where 

    import Grammata.Machine.Core.Class (GrammataCore (..), Ident, Pointer)
    import Grammata.Machine.Core.Types (Basic (..))

    import Control.Applicative ((<|>), pure, (<*>), (<$>))
    import Control.Monad (forM)

    -- | A imperative method (function or procedure) represented by local variables, parameters and it's code.
    data Method m = Method [(Ident, Expression m)] [Ident] [CoreStatement m] 


    -- | Imperative core language AST.
    data CoreStatement m = 
        -- | <IDENT> := <EXPR>
          Ident := Expression m
        -- | if <EXPR> then <STMT>* else <STMT>*
        | IIf (Expression m) [CoreStatement m] [CoreStatement m]
        -- | while <EXPR> do <STMT>* 
        | IWhile (Expression m) [CoreStatement m]
        -- | return <EXPR>
        | IReturn (Expression m)
        -- | call <IDENT> (<EXPR>*)
        | ICall Ident [Expression m]
        -- | trackback
        | TrackBack

    -- | Arithmetical Expression AST.
    data Expression m = 
        -- | <IDENT>
          Var Ident
        -- | <BASIC>
        | Val Basic
        -- | <OP> <BASIC>*
        | Op ([Basic] -> m Basic) [Expression m]
        -- | <IDENT> (<EXPR>*)
        | Func Ident [Expression m]

    -- | Imperative core language evaluation monad class.
    class GrammataCore m => CoreImperative m where
        -- | Entering a new scope.
        enter                   :: [(Ident, Basic)] -> m ()
        -- | Leaving the current scope.
        leave                   :: m ()
        -- | Calling and running a procedure.
        callProcedure           :: Ident -> [Basic] -> m ()
        -- | Reading from the stack.
        readStack               :: Ident -> m Basic
        -- | Writing to the stack, either locally or or generally.
        writeStack, writeLocals :: Ident -> Basic -> m ()
        -- | Backtracking.
        trackBack               :: m a
        trackBack = fail "BACKTRACK"

    -- | Transforms a list like @[[1],[2,3],[4],[5,6,7]]@ to @[[1,2,4,5],[1,2,4,6],[1,2,4,7],[1,3,4,5],[1,3,4,6],[1,3,4,7]]@.
    parallel :: () 
        => [[a]] -- ^ Input list.
        -> [[a]] -- ^ Output list.
    parallel [] = [[]]
    parallel (xs:xss) = [x:ys | x <- xs, ys <- parallel xss]

    -- | Evaluates an arithmetical expression.
    evalExpression :: (CoreImperative m)
        => [(Ident, Expression m)]  -- ^ Temporary auxiliary symbol table.
        -> Expression m             -- ^ Expression to evaluate.
        -> m [Basic]                -- ^ List of possible results.
    evalExpression tmp (Var id) = (readTemp tmp >>= evalExpression tmp) <|> (readStack id >>= return . return)
        where 
            readTemp tmp = case id `lookup` tmp of
                Nothing -> fail $ "Cannot find " ++ id ++ " in TMP."
                Just e  -> return e
    evalExpression _ (Val bsc) = return [bsc] 
    evalExpression tmp (Op f args) = mapM (evalExpression tmp) args >>= return . parallel >>= choice . map (\args -> f args >>= return . (:[])) 
    evalExpression tmp (Func name args) = mapM (evalExpression tmp) args >>= return . parallel >>= choice . map (callFunction name)

    -- | Evaluates a expression and extracts a boolean.
    evalToBoolean :: CoreImperative m 
        => Expression m -- ^ Expression to evaluate.
        -> m Bool       -- ^ Result.
    evalToBoolean cond = do
        conds <- evalExpression [] cond 
        choice . flip map conds $ \cond -> case cond of
            Boolean b -> return b
            others    -> fail $ "ERROR " ++ show others ++ " is no boolean."

    -- | Evaluates a expression and extracts an integer.
    evalToInteger :: CoreImperative m 
        => Expression m -- ^ Expression to evaluate.
        -> m Integer    -- ^ Result.
    evalToInteger cond = do
        conds <- evalExpression [] cond 
        choice . flip map conds $ \cond -> case cond of
            Natural b -> return b
            others    -> fail $ "ERROR " ++ show others ++ " is no natural."

    -- | Runs a method as a function taking its arguments and returing a result.
    runFunction :: CoreImperative m 
        => Method m     -- ^ Method to run.
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
            run [] = fail "ERROR no return"
            run (stmt:stmts) = case stmt of
                ident := expr  -> evalExpression [] expr >>= choice . map (\basic -> writeStack ident basic >> run stmts)
                IIf cond b1 b2 -> do
                    cond' <- evalToBoolean cond
                    if cond' then run (b1 ++ stmts) else run (b2 ++ stmts)
                IWhile cond block -> do 
                    cond' <- evalToBoolean cond 
                    if cond' then run $ block ++ stmt:stmts else run stmts
                IReturn expr -> evalExpression [] expr >>= choice . map return
                ICall ident exprs -> mapM (evalExpression []) exprs >>= return . parallel >>= choice . map (\args -> callProcedure ident args >> run stmts)
                TrackBack -> trackBack 

    -- | Runs a method as a procedure taking its arguments.
    runProcedure :: CoreImperative m 
        => Method m     -- ^ Method to run.
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
                        _ -> fail $ "ERROR exit code " ++ show n ++ "."
                ICall ident exprs -> mapM (evalExpression []) exprs >>= return . parallel >>= choice . map (\args -> callProcedure ident args >> run stmts)
                TrackBack -> trackBack 
