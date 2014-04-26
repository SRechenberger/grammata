{-|
Module      : Grammata
Description : Conversion of the AST to a runnable Execution type.
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : experimental
Portability : portable
-}

module Grammata 
(
    runScript
)
where

    import Grammata.Parser (parse)
    import Grammata.Parser.AST (Program (Program), 
        Declaration (Num, Func), 
        Statement ((:=), For, While, DoWhile, If, Return))
    import Grammata.Parser.Analysis (Analysis (LexicalError, SyntaxError, Parsed))
    import Grammata.Execution (declare, (.=), buildFunction, for, while, doWhile, ifThenElse, exitSuccess)

    import General (run, ExitState (Failure, Success), Execution)

    import Data.Foldable (forM_)

    import Control.Applicative ((<*>), (<$>))

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
                    Success res -> return $ "RESULT: " ++ show res 

    -- |Generates the program action
    interpret :: Program        -- ^ Program-AST
              -> Execution ()   -- ^ Executable Action
    interpret (Program decls stmts) = do 
        mapM_ interpretDecl decls 
        mapM_ interpretStmt stmts 

    -- |Generates a declaration action
    interpretDecl :: Declaration    -- ^ Declaration-AST
                  -> Execution ()   -- ^ Declaring function
    interpretDecl (Num id e) = do
        declare id
        forM_ e (id .=) 
    interpretDecl (Func id params decls stmts) = buildFunction id params $ do 
        mapM_ interpretDecl decls 
        mapM_ interpretStmt stmts 

    -- |Generates a statement action
    interpretStmt :: Statement      -- ^ Statement-AST
                  -> Execution ()   -- ^ Action executing the statement
    interpretStmt (id := expr) = id .= expr 
    interpretStmt (For id end step stmts) = for id end step . mapM_ interpretStmt $ stmts
    interpretStmt (While cond stmts) = while cond . mapM_ interpretStmt $ stmts
    interpretStmt (DoWhile cond stmts) = doWhile cond . mapM_ interpretStmt $ stmts
    interpretStmt (If cond th el) = ifThenElse cond (mapM_ interpretStmt th) (mapM_ interpretStmt el)
    interpretStmt (Return e) = exitSuccess e


