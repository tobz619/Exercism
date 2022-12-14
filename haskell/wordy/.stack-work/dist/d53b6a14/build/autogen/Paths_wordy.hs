{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_wordy (
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
version = Version [1,5,0,8] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Tobi\\Exercism\\haskell\\wordy\\.stack-work\\install\\b18ebbf0\\bin"
libdir     = "C:\\Users\\Tobi\\Exercism\\haskell\\wordy\\.stack-work\\install\\b18ebbf0\\lib\\x86_64-windows-ghc-9.0.2\\wordy-1.5.0.8-DCAj7y93Kkq848UJfRlkIn"
dynlibdir  = "C:\\Users\\Tobi\\Exercism\\haskell\\wordy\\.stack-work\\install\\b18ebbf0\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "C:\\Users\\Tobi\\Exercism\\haskell\\wordy\\.stack-work\\install\\b18ebbf0\\share\\x86_64-windows-ghc-9.0.2\\wordy-1.5.0.8"
libexecdir = "C:\\Users\\Tobi\\Exercism\\haskell\\wordy\\.stack-work\\install\\b18ebbf0\\libexec\\x86_64-windows-ghc-9.0.2\\wordy-1.5.0.8"
sysconfdir = "C:\\Users\\Tobi\\Exercism\\haskell\\wordy\\.stack-work\\install\\b18ebbf0\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "wordy_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wordy_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "wordy_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "wordy_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wordy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "wordy_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
