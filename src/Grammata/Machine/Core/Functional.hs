{-|
Module : Grammata.Machine.Core.Functional
Description : Grammata Functional Execution Core
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

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Grammata.Machine.Core.Functional
(
    
)
where
    
    import Data.List (intercalate)
    import Grammata.Machine.Core.Calls
    
    data MLambda ident basic = 
          BASIC basic
        | UNI (basic -> basic) (MLambda ident basic)
        | BIN (basic -> basic -> basic) (MLambda ident basic) (MLambda ident basic)
        | SYMBOL ident
        | CLOS basic
        | FUNC [ident] (MLambda ident basic)
        | APP (MLambda ident basic) [MLambda ident basic]
        | LETREC [(ident, MLambda ident basic)] (MLambda ident basic)
        | IF (MLambda ident basic) (MLambda ident basic) (MLambda ident basic)
        | CALL (Call ident basic)



    instance (Show ident, Show basic) => Show (MLambda ident basic) where
        show (BASIC b) = show b
        show (UNI _ e) = "(°" ++ show e ++ ")"
        show (BIN _ e1 e2) = "(" ++ show e1 ++ " ° " ++ show e2 ++ ")"
        show (SYMBOL s) = show s
        show (CLOS ptr) = "→" ++ show ptr 
        show (FUNC params expr) = "(Λ" ++ unwords (map show params) ++ "." ++ show expr ++ ")" 
        show (APP f args) = "(" ++ show f ++ " " ++ unwords (map show args) ++ ")"
        show (LETREC defs expr) = "(letrec " ++ intercalate "; " (map (\(id,expr) -> show id ++ " := " ++ show expr) defs) ++ " in " ++ show expr ++ ")"
        
    class (Monad m) => CoreFunctional m ident basic | m -> ident basic where
        loadHeapObj   :: basic -> m (MLambda ident basic)
        updateHeapObj :: basic -> (MLambda ident basic) -> m ()
        newClosure    :: (MLambda ident basic) -> m basic
        alloc         :: Int -> m [basic]
        call          :: Call ident basic -> m basic
        loadSymbol    :: ident -> m basic
        
    isRedex :: ()
            => MLambda ident basic 
            -> Bool
    isRedex (UNI _ _) = True
    isRedex (BIN _ _ _) = True
    isRedex (CLOS _) = True
    isRedex (APP _ _) = True
    isRedex (LETREC _ _) = True
    isRedex (IF cond _ _) = isRedex cond
    isRedex _ = False

    reduce :: (CoreFunctional m ident basic)
           => MLambda ident basic 
           -> m (MLambda ident basic)
    reduce expr = do 
        expr' <- case expr of
            APP f args -> mapM newClosure args >>= bind f 
        
        undefined


    bind :: (CoreFunctional m ident basic)
         => MLambda ident basic
         -> [basic]
         -> m (MLambda ident basic)
    bind = undefined

    getBasic :: (CoreFunctional m ident basic, Show ident, Show basic)
        => MLambda ident basic
        -> m basic
    getBasic expr = do 
        probB <- evaluate expr
        case probB of
            BASIC b   -> return b
            CLOS c    -> loadHeapObj c >>= getBasic
            SYMBOL s  -> loadSymbol s
            err       -> fail $ "ERROR could not reduce " ++ show err ++ " to a basic."       

    evaluate :: (CoreFunctional m ident basic) 
        => MLambda ident basic 
        -> m (MLambda ident basic)
    evaluate = undefined
