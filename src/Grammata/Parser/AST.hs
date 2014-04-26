{-|
Module      : Grammata.Parser.AST
Description : grammata syntax tree
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : stable
Portability : portable
-}

module Grammata.Parser.AST
(
    -- * Grammata program syntax tree
    Program (Program),

    -- ** Declarations
    Declaration (Num, Func),

    -- ** Statements
    Statement ((:=), For, While, DoWhile, If, Return),

    -- ** Expression Type
    Expression (Variable, Constant, Binary, Unary, Application),
)
where

    import Data.List (intercalate)
    import General (Execution, Identifier, Number, Function, Type)

    -- |Arithmetical expressions
    data Expression =
          Variable Identifier
        | Constant Number
        | Binary (Execution (Number -> Number -> Number)) Expression Expression
        | Unary (Execution (Number -> Number)) Expression
        | Application Identifier [Expression] 

    instance Show Expression where
        show (Variable id) = "id (" ++ id ++ ")"
        show (Constant c)  = "const (" ++ show c ++ ")"
        show (Binary _ e1 e2) = "(" ++ show e1 ++ " ° " ++ show e2 ++ ")"
        show (Unary _ e) = "(*" ++ show e ++ ")"
        show (Application fid es) = fid ++ "(" ++ intercalate "," (map show es) ++ ")"

    -- |program {...}
    data Program = Program [Declaration] [Statement]

    instance Show Program where
        show (Program ds ss) = "program { " ++ unwords (map show ds) ++ "; " ++ unwords (map show ss) ++ "}"

    -- |Declarations of functions and numbers
    data Declaration = 
        -- |num <Identifier> [:= <Expression>];
          Num Identifier (Maybe Expression)
        -- |func <Identifier> := func (num <Identifier>,...) {...};
        | Func Identifier [Identifier] [Declaration] [Statement]

    instance Show Declaration where
        show (Num id me) = id ++ case me of {Nothing -> ""; Just e -> show e} ++ ";"
        show (Func id ps ds ss) = id ++ "(num " ++ intercalate ", num " (map show ps) ++ ") {" ++ unwords (map show ds) ++ " " ++ unwords (map show ss) ++ "}"

    -- |Program Statements
    data Statement = 
        -- |<Identifier> := <Expression>;
          Identifier := Expression
        -- |for (<Identifier>; <Expression>; <Expression>) {...};
        | For Identifier Expression Expression [Statement]
        -- |while (<Expression>) {...};
        | While Expression [Statement]
        -- |do {...} while (<Expression>);
        | DoWhile Expression [Statement]
        -- |if (<Expression>) then {...} [else {...}];
        | If Expression [Statement] [Statement]
        -- |return <Expression>;
        | Return Expression

    instance Show Statement where
        show (id := e) = id ++ " := " ++ show e ++ ";"
        show (For i lim step ss) = "for (" ++ i ++ "; " ++ show lim ++ "; " ++ show step ++ ") {" ++ unwords (map show ss) ++ "};"
        show (While cond ss) = "while (" ++ show cond ++ ") {" ++ unwords (map show ss) ++ "};"
        show (DoWhile cond ss) = "do {" ++ unwords (map show ss) ++ "} while (" ++ show cond ++ ");"
        show (If cond th el) = "if (" ++ show cond ++ ") then {" ++ unwords (map show th) ++ "} else {" ++ unwords (map show el) ++ "};"
        show (Return e) = "return " ++ show e ++ ";"