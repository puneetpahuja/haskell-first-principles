{-# LANGUAGE CPP                #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_exercises (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception  as Exception
import           Data.Version       (Version (..))
import           Prelude
import           System.Environment (getEnv)

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/puneet/.cabal/bin"
libdir     = "/home/puneet/.cabal/lib/x86_64-linux-ghc-8.6.3/exercises-0.1.0.0-FAjAqdZxRmhK236iJhlxqS-exercises"
dynlibdir  = "/home/puneet/.cabal/lib/x86_64-linux-ghc-8.6.3"
datadir    = "/home/puneet/.cabal/share/x86_64-linux-ghc-8.6.3/exercises-0.1.0.0"
libexecdir = "/home/puneet/.cabal/libexec/x86_64-linux-ghc-8.6.3/exercises-0.1.0.0"
sysconfdir = "/home/puneet/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "exercises_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "exercises_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "exercises_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "exercises_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "exercises_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "exercises_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
