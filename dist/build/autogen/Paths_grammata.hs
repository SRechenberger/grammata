module Paths_grammata (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,4], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/garro/.cabal/bin"
libdir     = "/home/garro/.cabal/lib/grammata-0.1.0.4/ghc-7.6.3"
datadir    = "/home/garro/.cabal/share/grammata-0.1.0.4"
libexecdir = "/home/garro/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "grammata_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "grammata_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "grammata_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "grammata_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
