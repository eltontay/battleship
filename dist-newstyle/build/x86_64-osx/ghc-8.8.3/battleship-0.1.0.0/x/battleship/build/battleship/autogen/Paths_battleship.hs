{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_battleship (
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

bindir     = "/Users/eltontay/.cabal/bin"
libdir     = "/Users/eltontay/.cabal/lib/x86_64-osx-ghc-8.8.3/battleship-0.1.0.0-inplace-battleship"
dynlibdir  = "/Users/eltontay/.cabal/lib/x86_64-osx-ghc-8.8.3"
datadir    = "/Users/eltontay/.cabal/share/x86_64-osx-ghc-8.8.3/battleship-0.1.0.0"
libexecdir = "/Users/eltontay/.cabal/libexec/x86_64-osx-ghc-8.8.3/battleship-0.1.0.0"
sysconfdir = "/Users/eltontay/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "battleship_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "battleship_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "battleship_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "battleship_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "battleship_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "battleship_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
