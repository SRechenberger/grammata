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
    emptyEnv, initializeEnv,
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
    
    -- |Classifies a type as initially evaluatable.
    class Initializable init where
        -- |Initially evaluates the initial expression saved on the stack bottom.
        initialize :: Monad m => init -> m init

    -- |@Environment@ type, respresenting the static structure, with dynamic incarnation management.
    data Environment key content = Env {
            local  :: [(key, [content])],    -- ^ Locally visible values. Dynamically stacked.
            hidden :: [(key, Environment key content)]  -- ^ Subscopes.
        } deriving (Eq)

    instance (Show key, Show content) => Show (Environment key content) where
        show (Env l h) = intercalate "\n" (map (\(k,p) -> show k ++ ":" ++ show p) l) ++ "\n" ++ intercalate " " (map (" "++) . map (\(k,e) -> show k ++ "." ++ show e) $ h)

    -- |Creates a new empty @Environment@.
    emptyEnv :: Eq key => Environment key content -- ^ Empty @Environment@.
    emptyEnv = Env [] []

    -- |Writes a value into an @Environment@, following a path of keys to the last key, which is the key of the actual value, whereas the former keys are keys of subscopes.
    writeEnv :: (Monad m, Eq key, Show key, Show content) => Path key                        -- ^ The path to the value to be written.
                                                          -> (content -> content -> Bool)    -- ^ A relation which hast to hold, if the value should be written.
                                                          -> content                         -- ^ The value to be written.
                                                          -> Environment key content         -- ^ The @Environment@ to be modified.
                                                          -> m (Environment key content)     -- ^ The resulting @Environment@. Yields @Nothing@ if the relation didn't hold.
    writeEnv [] _ _ _ = fail "Empty path."
    writeEnv [key] rel c env = case lookup key . local $ env of 
        Nothing -> return $ env {local = (key, [c]) : local env}
        Just _  -> do 
            newLoc <- mapM (\(key', c':cs') -> (if key' == key then (if c' `rel` c then return (key, c:cs') else fail $ "Cannot write " ++ show c ++ " to " ++ show key ++ ". Condition failed.") else return (key', c':cs'))) (local env) 
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
    readEnv :: (Show key, Eq key, Monad m) => Path key                -- ^ The path localizing the value to be read.
                                           -> Environment key content -- ^ The @Environment@ containing the value.
                                           -> m content           -- ^ The value read. Yields @Nothing@ if the value cant be found.
    readEnv [] _ = fail "Empty path."
    readEnv [key] env = case lookup key . local $ env of
            Nothing    -> fail $ "Could not read path " ++ show key ++ "."
            Just (c:_) -> return c
    readEnv (key:keys) env = case lookup key . hidden $ env of 
            Nothing   -> fail $ "Cold not read Path " ++ show key ++ "."
            Just env' -> readEnv keys env'


    -- |Checks whether the existing path hast been declared.
    exists :: (Show key, Eq key) => Path key                -- ^ Path to check.
                                 -> Environment key content -- ^ @Environment@ to check.
                                 -> Bool                    -- ^ True if the path has been declared.
    exists path env = case readEnv path env of
        Nothing -> False
        Just _  -> True

    -- |Initializes a new incarnation of all values of the scope localized by the given path with its default value.
    enterScope :: (Show key, Eq key, Monad m) => Path key                        -- ^ Path of the entered Scope.
                                              -> Environment key content         -- ^ Used @Environment@.
                                              -> m (Environment key content)     -- ^ Modified @Environment@. Yields nothing, if the scope does not exist.
    enterScope [] env = mapM (\(key, cs) -> return cs >>= return . last >>= \c -> return (key, c:cs)) (local env) >>= \newLoc -> return $ env {local = newLoc}
    enterScope (key:path) env = case lookup key . hidden $ env of
        Nothing   -> fail $ "Could not enter scope " ++ (intercalate "." (map show $ key:path)) ++ "."
        Just env' -> enterScope path env' >>= \env' -> return $ env {hidden = map (\(key', e) -> if key' == key then (key, env') else (key', e)) . hidden $ env}

    -- |Wipes one incarnation of all values of the scope localized by the given path.
    leaveScope :: (Show key, Eq key, Monad m) => Path key                        -- ^ Path of the left scope.
                                              -> Environment key content         -- ^ Used @Environment@.
                                              -> m (Environment key content) -- ^ Modified @Environment@. Yields nothing, if the scope does not exist. 
    leaveScope [] env = return $ env {local = map (\(key, _:cs) -> (key, cs)) . local $ env} 
    leaveScope (key:path) env = case lookup key . hidden $ env of
        Nothing   -> fail $ "Could not leave scope " ++ show (intercalate "." (map show $ key:path)) ++ "."
        Just env' -> leaveScope path env' >>= \env' -> return $ env {hidden = map (\(key', e) -> if key' == key then (key, env') else (key, e)) . hidden $ env}

    -- |Creates an initialized @Environment@ according to a list of pairs of a path and a default value.
    initializeEnv :: (Eq key, Monad m, Show key, Show content) => [(Path key, content)]           -- ^ List of pairs of a path and a default value
                                                               -> m (Environment key content) -- ^ Initialized @Environment@.
    initializeEnv initVals = init' initVals emptyEnv
        where 
            init' :: (Eq key, Monad m, Show key, Show content) => [(Path key, content)] 
                                                               -> Environment key content
                                                               -> m (Environment key content)
            init' [] env = return env
            init' ((p,c):initVals) env = writeEnv p uncond c env >>= init' initVals

    -- |Finds the paths of all occuring values identified by the second parameter on the given path in the given @Environment@.
    findDeclarations :: (Show key, Eq key) => Path key                -- ^ Path to be searched.
                                           -> key                     -- ^ The identifier to look after.
                                           -> Environment key content -- ^ The @Environment@ to be searched.
                                           -> [[key]]               -- ^ All paths of an occuring of the identifier.
    findDeclarations path id env = filter (flip exists env) . map (++ [id]) . inits $ path
