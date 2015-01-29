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
-- Copyright : (c) Sascha Rechenberger, 2014, 2015
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

    import Data.List (intercalate)


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

    instance Show (CoreExpression m) where
        show (IVar i) = "$" ++ show i 
        show (IVal b) = show b 
        show (IOp _ es) = "°(" ++ intercalate " ° " (map show es) ++ ")"
        show (IFunc i es) = "call " ++ i ++ "(" ++ intercalate ", " (map show es) ++ ")"
        show IRemind = "remind" 

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

