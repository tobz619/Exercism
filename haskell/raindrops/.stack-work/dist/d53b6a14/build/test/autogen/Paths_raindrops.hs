{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_raindrops (
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
version = Version [1,1,0,6] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Tobi\\Exercism\\haskell\\raindrops\\.stack-work\\install\\b18ebbf0\\bin"
libdir     = "C:\\Users\\Tobi\\Exercism\\haskell\\raindrops\\.stack-work\\install\\b18ebbf0\\lib\\x86_64-windows-ghc-9.0.2\\raindrops-1.1.0.6-Afrk69I5rwxKLvUWyh5Pj9-test"
dynlibdir  = "C:\\Users\\Tobi\\Exercism\\haskell\\raindrops\\.stack-work\\install\\b18ebbf0\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "C:\\Users\\Tobi\\Exercism\\haskell\\raindrops\\.stack-work\\install\\b18ebbf0\\share\\x86_64-windows-ghc-9.0.2\\raindrops-1.1.0.6"
libexecdir = "C:\\Users\\Tobi\\Exercism\\haskell\\raindrops\\.stack-work\\install\\b18ebbf0\\libexec\\x86_64-windows-ghc-9.0.2\\raindrops-1.1.0.6"
sysconfdir = "C:\\Users\\Tobi\\Exercism\\haskell\\raindrops\\.stack-work\\install\\b18ebbf0\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "raindrops_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "raindrops_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "raindrops_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "raindrops_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "raindrops_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "raindrops_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
