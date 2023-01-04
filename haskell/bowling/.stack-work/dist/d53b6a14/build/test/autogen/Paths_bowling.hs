{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_bowling (
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
version = Version [1,2,0,7] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\to4\\Documents\\Exercism\\haskell\\bowling\\.stack-work\\install\\1797caad\\bin"
libdir     = "C:\\Users\\to4\\Documents\\Exercism\\haskell\\bowling\\.stack-work\\install\\1797caad\\lib\\x86_64-windows-ghc-9.0.2\\bowling-1.2.0.7-AnEikzOzwoh3N9wyDJ3OfZ-test"
dynlibdir  = "C:\\Users\\to4\\Documents\\Exercism\\haskell\\bowling\\.stack-work\\install\\1797caad\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "C:\\Users\\to4\\Documents\\Exercism\\haskell\\bowling\\.stack-work\\install\\1797caad\\share\\x86_64-windows-ghc-9.0.2\\bowling-1.2.0.7"
libexecdir = "C:\\Users\\to4\\Documents\\Exercism\\haskell\\bowling\\.stack-work\\install\\1797caad\\libexec\\x86_64-windows-ghc-9.0.2\\bowling-1.2.0.7"
sysconfdir = "C:\\Users\\to4\\Documents\\Exercism\\haskell\\bowling\\.stack-work\\install\\1797caad\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bowling_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bowling_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "bowling_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "bowling_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bowling_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bowling_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
