{-|
Module      : General.Environment
Description : General grammata-Script variable environment.
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : stable
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

module General.Environment 
(
    -- * @Environment@ Type
    Environment,
    -- ** Creating new @Environment@s
    emptyEnv, initialize,
    -- ** Modifying @Environment@s
    writeEnv, readEnv,
    -- ** Entering and leaving scopes.
    enterScope, leaveScope,

    -- ** Finding stuff
    findDeclarations, exists,

    -- * Utilities
    uncond
)
where

    import Debug.Trace
    import Data.List (intercalate, inits)
    import Control.Applicative ((<$>))

    -- |Path in an @Environment@.
    type Path key = [key]
    
    
    -- |@Environment@ type, respresenting the static structure, with dynamic incarnation management.
    data Environment key content = Env {
            local  :: [(key, [content])],    -- ^ Locally visible values. Dynamically stacked.
            hidden :: [(key, Environment key content)]  -- ^ Subscopes.
        } deriving (Eq)

    instance (Show key, Show content) => Show (Environment key content) where
        show (Env l h) = intercalate " " (map show l) ++ (if null h || null l then "" else ":") ++ intercalate " " (map show h)

    -- |Creates a new empty @Environment@.
    emptyEnv :: Eq key => Environment key content -- ^ Empty @Environment@.
    emptyEnv = Env [] []

    -- |Writes a value into an @Environment@, following a path of keys to the last key, which is the key of the actual value, whereas the former keys are keys of subscopes.
    writeEnv :: Eq key => Path key                        -- ^ The path to the value to be written.
                       -> (content -> content -> Bool)    -- ^ A relation which hast to hold, if the value should be written.
                       -> content                         -- ^ The value to be written.
                       -> Environment key content         -- ^ The @Environment@ to be modified.
                       -> Maybe (Environment key content) -- ^ The resulting @Environment@. Yields @Nothing@ if the relation didn't hold.
    writeEnv [] _ _ _ = Nothing
    writeEnv [key] rel c env = case lookup key . local $ env of 
        Nothing -> return $ env {local = (key, [c]) : local env}
        Just _  -> do 
            newLoc <- mapM (\(key', c':cs') -> (if key' == key then (if c' `rel` c then Just (key, c:cs') else Nothing) else Just (key', c':cs'))) (local env) 
            return $ env {local = newLoc}
    writeEnv (key:keys) rel c env = case lookup key . hidden $ env of
        Nothing   -> writeEnv keys rel c emptyEnv >>= \env' -> return $ env {hidden = (key, env'):hidden env}
        Just env' -> writeEnv keys rel c env' >>= \env'' -> return $ env {hidden = map (\(key', e) -> if key' == key then (key, env'') else (key', e)) $ hidden env}

    -- |Binary relation which is always true.
    uncond :: a     -- ^ Ignored parameter 1.
           -> b     -- ^ Ignored parameter 2.
           -> Bool  -- ^ @True@.
    uncond _ _ = True

    -- |Reads a value identified by the given Path.
    readEnv :: Eq key => Path key                -- ^ The path localizing the value to be read.
                      -> Environment key content -- ^ The @Environment@ containing the value.
                      -> Maybe content           -- ^ The value read. Yields @Nothing@ if the value cant be found.
    readEnv [] _ = Nothing
    readEnv [key] env = return env >>= lookup key . local >>= return . head
    readEnv (key:keys) env = return env >>= lookup key . hidden >>= readEnv keys

    -- |Checks whether the existing path hast been declared.
    exists :: Eq key => Path key                -- ^ Path to check.
                     -> Environment key content -- ^ @Environment@ to check.
                     -> Bool                    -- ^ True if the path has been declared.
    exists path env = case readEnv path env of
        Nothing -> False
        Just _  -> True

    -- |Initializes a new incarnation of all values of the scope localized by the given path with its default value.
    enterScope :: Eq key => Path key                        -- ^ Path of the entered Scope.
                         -> Environment key content         -- ^ Used @Environment@.
                         -> Maybe (Environment key content) -- ^ Modified @Environment@. Yields nothing, if the scope does not exist.
    enterScope [] env = return $ env {local = map (\(key, cs) -> (key, (last cs:cs))) . local $ env}
    enterScope (key:path) env = case lookup key . hidden $ env of
        Nothing   -> Nothing
        Just env' -> enterScope path env' >>= \env' -> return $ env {hidden = map (\(key', e) -> if key' == key then (key, env') else (key', e)) . hidden $ env}

    -- |Wipes one incarnation of all values of the scope localized by the given path.
    leaveScope :: Eq key => Path key                        -- ^ Path of the left scope.
                         -> Environment key content         -- ^ Used @Environment@.
                         -> Maybe (Environment key content) -- ^ Modified @Environment@. Yields nothing, if the scope does not exist. 
    leaveScope [] env = return $ env {local = map (\(key, _:cs) -> (key, cs)) . local $ env} 
    leaveScope (key:path) env = case lookup key . hidden $ env of
        Nothing   -> Nothing
        Just env' -> leaveScope path env' >>= \env' -> return $ env {hidden = map (\(key', e) -> if key' == key then (key, env') else (key, e)) . hidden $ env}

    -- |Creates an initialized @Environment@ according to a list of pairs of a path and a default value.
    initialize :: Eq key => [(Path key, content)]           -- ^ List of pairs of a path and a default value
                         -> Maybe (Environment key content) -- ^ Initialized @Environment@.
    initialize initVals = init' initVals emptyEnv
        where 
            init' :: Eq key => [(Path key, content)] 
                            -> Environment key content
                            -> Maybe (Environment key content)
            init' [] env = return env
            init' ((p,c):initVals) env = writeEnv p uncond c env >>= init' initVals

    -- |Finds the paths of all occuring values identified by the second parameter on the given path in the given @Environment@.
    findDeclarations :: Eq key => Path key                -- ^ Path to be searched.
                               -> key                     -- ^ The identifier to look after.
                               -> Environment key content -- ^ The @Environment@ to be searched.
                               -> Maybe [[key]]           -- ^ All paths of an occuring of the identifier.
    findDeclarations path id env = return . filter (flip exists env) . map (++ [id]) . inits $ path
