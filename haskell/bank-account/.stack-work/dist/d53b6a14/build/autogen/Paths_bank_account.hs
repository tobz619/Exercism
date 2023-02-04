{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_bank_account (
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
version = Version [0,1,0,4] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Tobi\\Exercism\\haskell\\bank-account\\.stack-work\\install\\b18ebbf0\\bin"
libdir     = "C:\\Users\\Tobi\\Exercism\\haskell\\bank-account\\.stack-work\\install\\b18ebbf0\\lib\\x86_64-windows-ghc-9.0.2\\bank-account-0.1.0.4-4TnTAy5KpnhBOwCMhkOzTe"
dynlibdir  = "C:\\Users\\Tobi\\Exercism\\haskell\\bank-account\\.stack-work\\install\\b18ebbf0\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "C:\\Users\\Tobi\\Exercism\\haskell\\bank-account\\.stack-work\\install\\b18ebbf0\\share\\x86_64-windows-ghc-9.0.2\\bank-account-0.1.0.4"
libexecdir = "C:\\Users\\Tobi\\Exercism\\haskell\\bank-account\\.stack-work\\install\\b18ebbf0\\libexec\\x86_64-windows-ghc-9.0.2\\bank-account-0.1.0.4"
sysconfdir = "C:\\Users\\Tobi\\Exercism\\haskell\\bank-account\\.stack-work\\install\\b18ebbf0\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bank_account_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bank_account_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "bank_account_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "bank_account_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bank_account_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bank_account_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
