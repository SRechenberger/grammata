{-|
Module      : Grammata
Description : Conversion of the AST to a runnable Execution type.
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : experimental
Portability : portable
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3

This file is part of grammata.

grammata is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

grammata is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with grammata. If not, see <http://www.gnu.org/licenses/>.
-}

module Grammata 
(
    execute
)
where

    import Debug.Trace (trace)

    import Grammata.Parser (parse)
    import Grammata.Parser.AST (Program (Program), 
        Declaration (Var, Num, Func, Proc), 
        Statement ((:=), For, While, DoWhile, If, Return, Call),
        Arithmetical (Id, Con, Bin, Un, App))
    import Grammata.Parser.Analysis (Analysis (LexicalError, SyntaxError, SemanticalError, Parsed))
    import Grammata.Execution (buildFunction, for, while, doWhile, ifThenElse, buildProcedure)
    import General (
        traceLog,
        runScript, 
        ExitState (Failure, Success), 
        Grammata, 
        Type (Null, Number, Formal, Function, Procedure), 
        getTable, putTable, 
        Identifier, 
        Number, 
        loadValue, storeValue,
        deformalize, 
        exitSuccess, exitFailing)
    import General.Environment (initializeEnv, Environment, findDeclarations, uncond, writeEnv, emptyEnv, exists)
    import General.Expression (Expression (Variable, Constant, Binary, Unary, Application), EvalApparatus (..))
    import Data.Foldable (forM_)

    import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)

    import Control.Applicative ((<*>), (<$>))
    import Control.Monad.IO.Class (liftIO)
    import Control.Concurrent.MVar (putMVar, newEmptyMVar)

    -- |Actually runs a script given as a string, returning a string representing the result or an error Message.
    execute :: String       -- ^ Input Script.
            -> IO String    -- ^ Result or error message.
    execute script = do 
        case parse script >>= interpret of
            LexicalError l -> return $ "Lexical Error " ++ l
            SyntaxError s  -> return $ "Syntax Error" ++ s 
            SemanticalError s -> return $ "Semantical Error" ++ s
            Parsed action -> do
                result <- runScript action emptyEnv
                case result of 
                    Success n -> return $ "Result " ++ show n
                    Failure f -> return $ f

    -- |Converts the abstract syntax tree to an executable Grammata action.
    interpret :: Program Identifier (Arithmetical Identifier Number String) -- ^ the AST to compile.
              -> Analysis String String String (Grammata ())                -- ^ the resulting action.
    interpret p@(Program decls stmts) = do 
        let analyzedDecls = analyzeDecls [] decls
        environment <- initializeEnv . flip zip (repeat Null) . fst . unzip $ analyzedDecls
        let convertedDecls = map (convertExpressions environment) analyzedDecls
        static <- foldl (\envM (p,c) -> envM >>= \env -> writeEnv p uncond c env) (return environment) convertedDecls
        let analyzedProgram = analyze p static
        return $ compileToMonadicAction analyzedProgram static 

    -- |Compiles a transformed an AST and an Environment to an executable Grammata action.
    compileToMonadicAction :: Program [Identifier] Type     -- ^ The AST to compile.
                           -> Environment Identifier Type   -- ^ The initial environment.
                           -> Grammata ()                   -- ^ The resulting action.
    compileToMonadicAction (Program decls stmts) env = putTable env >> compileDecls decls >> compileStmts stmts
        where
            compileDecls :: [Declaration [Identifier] Type] -> Grammata () 
            compileDecls [] = return ()
            compileDecls (decl:decls) = action >> compileDecls decls
                where
                    action = case decl of
                        Num path _                 -> loadValue path >>= deformalize >>= storeValue path
                        Func path args decls stmts -> storeValue path $ buildFunction path args (compileDecls decls >> compileStmts stmts)  
                        Proc path args decls stmts -> storeValue path $ buildProcedure path args (compileDecls decls >> compileStmts stmts)
                        _                          -> return ()

            compileStmts :: [Statement [Identifier] Type] -> Grammata ()
            compileStmts [] = return ()
            compileStmts (stmt:stmts) = action >> compileStmts stmts
                where
                    action = case stmt of
                        path := expr                              -> deformalize expr >>= storeValue path
                        For path (Formal fin) (Formal step) stmts -> for path fin step (compileStmts stmts)
                        While (Formal cond) stmts                 -> while cond (compileStmts stmts)
                        DoWhile (Formal cond) stmts               -> doWhile cond (compileStmts stmts)
                        If (Formal cond) stmts1 stmts2            -> ifThenElse cond (compileStmts stmts1) (compileStmts stmts2)
                        Call path args                            -> loadValue path >>= \p -> case p of 
                            Procedure p -> mapM deformalize args >>= p 
                            val         -> exitFailing $ show val ++ " is no Procedure."
                        Return (Formal e)                         -> exitSuccess e

    -- |Analyzes the static structure of a program's declarations.
    analyzeDecls :: [Identifier]                                                      -- ^ The path to a certain point.
                 -> [Declaration Identifier (Arithmetical Identifier Number String)]  -- ^ Declarations to analyze.
                 -> [([Identifier], Maybe (Arithmetical Identifier Number String))]   -- ^ The static structure of the declarions.
    analyzeDecls _ [] = []
    analyzeDecls hither (decl:decls) = s ++ analyzeDecls hither decls 
        where 
            s = case decl of
                Var id            -> [(hither ++ [id], Nothing)]
                Num id e          -> [(hither ++ [id], Just e)]
                Func id args decls _ -> (hither ++ [id], Nothing):(map (\id' -> (hither ++ [id, id'], Nothing)) args ++ analyzeDecls (hither ++ [id]) decls)
                Proc id args decls _ -> (hither ++ [id], Nothing):(map (\id' -> (hither ++ [id, id'], Nothing)) args ++ analyzeDecls (hither ++ [id]) decls)

    -- |Transforms a Grammata AST and an Environment to a Grammata AST with resolved identifiers.
    analyze :: Program Identifier (Arithmetical Identifier Number String)  -- ^ AST to resolve.
            -> Environment Identifier Type                                 -- ^ Initial environment.
            -> Program [Identifier] Type                                   -- ^ Resolved AST.
    analyze (Program decls stmts) static = Program (analyzeNestedStatements [] static decls) (analyzeStatements [] static stmts)  
        where
            analyzeStatements :: [Identifier] 
                              -> Environment Identifier Type 
                              -> [Statement Identifier (Arithmetical Identifier Number String)] 
                              -> [Statement [Identifier] Type]
            analyzeStatements _ _ [] = []
            analyzeStatements hither env (stmt:stmts) = s : analyzeStatements hither env stmts 
                where
                    s = case stmt of
                        id := expr -> (last . findDeclarations hither id $ env) := let (_, e) = convertExpressions env (hither ++ [id], Just expr) in e
                        For i e1 e2 stmts -> let 
                            (_, e1') = convertExpressions env (hither ++ [i], Just e1) 
                            (_, e2') = convertExpressions env (hither ++ [i], Just e2) 
                            in For (hither ++ [i]) e1' e2' (analyzeStatements hither env stmts)
                        While e stmts -> let
                            (_, e') = convertExpressions env (hither, Just e) 
                            in While e' (analyzeStatements hither env stmts)
                        DoWhile e stmts -> let
                            (_, e') = convertExpressions env (hither, Just e) 
                            in DoWhile e' (analyzeStatements hither env stmts)
                        If e stmts1 stmts2 -> let
                            (_, e') = convertExpressions env (hither, Just e) 
                            in If e' (analyzeStatements hither env stmts1) (analyzeStatements hither env stmts2)
                        Call id exprs -> let 
                            exprs' = snd . unzip $ map (convertExpressions env) (zip (repeat hither) (map Just exprs))
                            in Call (last . findDeclarations hither id $ env) exprs'
                        Return e -> let (_, e') = convertExpressions env (hither, Just e) in Return e'

            analyzeNestedStatements :: [Identifier] 
                                    -> Environment Identifier Type 
                                    -> [Declaration Identifier (Arithmetical Identifier Number String)] 
                                    -> [Declaration [Identifier] Type]
            analyzeNestedStatements _ _ [] = []
            analyzeNestedStatements hither env (decl:decls) = s : analyzeNestedStatements hither env decls
                where
                    s = case decl of
                        Var id -> Var (last . findDeclarations hither id $ env)  
                        Num id e -> let (_, e') = convertExpressions env (hither ++ [id], Just e) in Num (last . findDeclarations hither id $ env) e'
                        Func id args decls' stmts' -> let args' = map (\id' -> (last . findDeclarations (hither++[id]) id' $ env)) args 
                            in Func (last . findDeclarations hither id $ env) args' (analyzeNestedStatements (hither ++ [id]) env decls') (analyzeStatements (hither ++ [id]) env stmts')
                        Proc id args decls' stmts' -> let args' = map (\id' -> (last . findDeclarations (hither++[id]) id' $ env)) args 
                            in Proc (last . findDeclarations hither id $ env) args' (analyzeNestedStatements (hither ++ [id]) env decls') (analyzeStatements (hither ++ [id]) env stmts')



    -- |Converts an Arithmetical AST into a Type using the given Environment.
    convertExpressions :: Environment Identifier Type                                   -- ^ The Environment.
                       -> ([Identifier], Maybe (Arithmetical Identifier Number String)) -- ^ The Expression to convert and its position withing the Environment.
                       -> ([Identifier], Type)                                          -- ^ The resolved Expression.
    convertExpressions env (path, Nothing) = (path, Null)
    convertExpressions env (path, Just e) = (path, Formal . resolve $ e)
        where 
            getLastDeclaration :: Identifier 
                               -> [Identifier]
            getLastDeclaration id = last $ findDeclarations path id env 

            resolve :: Arithmetical Identifier Number String 
                    -> Expression [Identifier] Type
            resolve (Id id) = Variable . getLastDeclaration $ id
            resolve (Con c) = Constant . Number $ c
            resolve (Bin op e1 e2) = let 
                f = case op of
                    "+" -> (+)
                    "-" -> (-)
                    "*" -> (*)
                    "div" -> \a b -> toEnum $ fromEnum a `div` fromEnum b
                    "/" -> (/)
                    "mod" -> (\a b -> toEnum $ fromEnum a `mod` fromEnum b)
                    ">=" -> \a b -> if a >= b then 1 else 0
                    ">" -> \a b -> if a > b then 1 else 0
                    "<=" -> \a b -> if a <= b then 1 else 0
                    "<" -> \a b -> if a < b then 1 else 0
                    "==" -> \a b -> if a == b then 1 else 0
                    "!=" -> \a b -> if a /= b then 1 else 0 
                in Binary (\(Number a) (Number b) -> Number $ a `f` b) (resolve e1) (resolve e2)
            resolve (Un op e) = let 
                f = case op of
                    "-" -> \(Number x) -> negate x
                    "not" -> \(Number x) -> if x > 0 then 0 else 1
                in Unary (Number . f) (resolve e)
            resolve (App id exprs) = Application (getLastDeclaration $ id) (map resolve exprs)


