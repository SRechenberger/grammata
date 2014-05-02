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
    runScript
)
where

    import Grammata.Parser (parse)
    import Grammata.Parser.AST (Program (Program), 
        Declaration (Var, Num, Func), 
        Statement ((:=), For, While, DoWhile, If, Return))
    import Grammata.Parser.Analysis (Analysis (LexicalError, SyntaxError, Parsed))
    import Grammata.Execution (declare, assign, (.=), buildFunction, for, while, doWhile, ifThenElse, exitSuccess, eval)
    import General (run, ExitState (Failure, Success), Execution, Type (Null, Number, Function, Procedure), get)

    import Data.Foldable (forM_)

    import Control.Applicative ((<*>), (<$>))
    import Control.Monad.IO.Class (liftIO)
    import Control.Concurrent.MVar (putMVar, newEmptyMVar)

    -- |Runs a grammata script returning the result or error message as a string.
    runScript :: String     -- ^ Script to run.
              -> IO String  -- ^ Result.
    runScript input = do
        let analysis = parse input
        case analysis of
            LexicalError lex -> return $ "LEXICAL ERROR " ++ lex
            SyntaxError syn  -> return $ "SYNTACTICAL ERROR " ++ syn 
            Parsed program   -> do
                result <- run (interpret program) [] 
                case result of
                    Failure err -> return $ "RUNTIME ERROR " ++ err
                    Success res -> return . show $ res 

    -- |Generates the program action
    interpret :: Program        -- ^ Program-AST
              -> Execution ()   -- ^ Executable Action
    interpret (Program decls stmts) = do 
        mapM_ interpretDecl decls 
        mapM_ interpretStmt stmts 

    -- |Generates a declaration action
    interpretDecl :: Declaration    -- ^ Declaration-AST
                  -> Execution ()   -- ^ Declaring function
    interpretDecl (Var id) = declare id
    interpretDecl (Num id e) = do
        declare id
        eval e >>= assign id
    interpretDecl (Func id params decls stmts) = do
        declare id
        static <- liftIO newEmptyMVar  
        assign id . buildFunction static params $ do 
            mapM_ interpretDecl decls 
            mapM_ interpretStmt stmts 
        get >>= liftIO . putMVar static

    -- |Generates a statement action
    interpretStmt :: Statement      -- ^ Statement-AST
                  -> Execution ()   -- ^ Action executing the statement
    interpretStmt (id := expr)            = eval expr >>= (id .=)
    interpretStmt (For id end step stmts) = for id end step . mapM_ interpretStmt $ stmts
    interpretStmt (While cond stmts)      = while cond . mapM_ interpretStmt $ stmts
    interpretStmt (DoWhile cond stmts)    = doWhile cond . mapM_ interpretStmt $ stmts
    interpretStmt (If cond th el)         = ifThenElse cond (mapM_ interpretStmt th) (mapM_ interpretStmt el)
    interpretStmt (Return e)              = exitSuccess e


