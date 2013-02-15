module Paths_HydraLib (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,8,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/users/level4/0801585s/.cabal/bin"
libdir     = "/users/level4/0801585s/.cabal/lib/HydraLib-0.8.1/ghc-7.0.3"
datadir    = "/users/level4/0801585s/.cabal/share/HydraLib-0.8.1"
libexecdir = "/users/level4/0801585s/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "HydraLib_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "HydraLib_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "HydraLib_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "HydraLib_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
