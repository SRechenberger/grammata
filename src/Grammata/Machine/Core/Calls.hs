{-|
Module : Grammata.Machine.Core.Call
Description : Grammata Calls between paradigms
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

module Grammata.Machine.Core.Calls
(
    Call (..),
    FunctionCall (..),
    QueryCall(..),
    Goals(..)
)
where
    
    import Data.List (intercalate)
    
    data Call ident basic = 
          FUNCTIONAL (FunctionCall ident basic)
        | LOGICAL (QueryCall ident basic)
        | IMPERATIVE (FunctionCall ident basic)
    
    data FunctionCall ident basic = FCall ident [basic]
    
    data QueryCall ident basic = QCall [ident] (Goals ident basic)
    
    instance (Show ident, Show basic) => Show (QueryCall ident basic) where
        show (QCall bases goals) = intercalate "," (map show bases) ++ " ?- " ++ show goals
    
    data Goals ident basic = 
          GOAL ident [Either ident basic] 
        | NOT (Goals ident basic) 
        | AND [Goals ident basic]
        | OR [Goals ident basic]
        
    instance (Show ident, Show basic) => Show (Goals ident basic) where
        show (GOAL name args) = show name ++ "(" ++ intercalate "," (map printarg args) ++ ")"
            where 
                printarg (Left var) = show var
                printarg (Right atom) = show atom
        show (NOT goal) = "¬" ++ show goal
        show (AND goals) = "(" ++ intercalate "," (map show goals) ++ ")"
        show (OR goals) = "(" ++ intercalate ";" (map show goals) ++ ")"  
    
