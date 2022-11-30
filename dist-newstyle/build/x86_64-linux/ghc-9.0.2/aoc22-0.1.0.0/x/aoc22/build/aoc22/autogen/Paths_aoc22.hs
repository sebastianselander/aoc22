{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_aoc22 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

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

bindir     = "/home/sebastian/.cabal/bin"
libdir     = "/home/sebastian/.cabal/lib/x86_64-linux-ghc-9.0.2/aoc22-0.1.0.0-inplace-aoc22"
dynlibdir  = "/home/sebastian/.cabal/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/sebastian/.cabal/share/x86_64-linux-ghc-9.0.2/aoc22-0.1.0.0"
libexecdir = "/home/sebastian/.cabal/libexec/x86_64-linux-ghc-9.0.2/aoc22-0.1.0.0"
sysconfdir = "/home/sebastian/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "aoc22_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "aoc22_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "aoc22_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "aoc22_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aoc22_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aoc22_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
