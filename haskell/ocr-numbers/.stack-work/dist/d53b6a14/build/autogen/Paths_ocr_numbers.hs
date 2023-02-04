{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_ocr_numbers (
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
version = Version [1,2,0,6] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Tobi\\Exercism\\haskell\\ocr-numbers\\.stack-work\\install\\b18ebbf0\\bin"
libdir     = "C:\\Users\\Tobi\\Exercism\\haskell\\ocr-numbers\\.stack-work\\install\\b18ebbf0\\lib\\x86_64-windows-ghc-9.0.2\\ocr-numbers-1.2.0.6-JCzbyEkgUxVC7m27b94wID"
dynlibdir  = "C:\\Users\\Tobi\\Exercism\\haskell\\ocr-numbers\\.stack-work\\install\\b18ebbf0\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "C:\\Users\\Tobi\\Exercism\\haskell\\ocr-numbers\\.stack-work\\install\\b18ebbf0\\share\\x86_64-windows-ghc-9.0.2\\ocr-numbers-1.2.0.6"
libexecdir = "C:\\Users\\Tobi\\Exercism\\haskell\\ocr-numbers\\.stack-work\\install\\b18ebbf0\\libexec\\x86_64-windows-ghc-9.0.2\\ocr-numbers-1.2.0.6"
sysconfdir = "C:\\Users\\Tobi\\Exercism\\haskell\\ocr-numbers\\.stack-work\\install\\b18ebbf0\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ocr_numbers_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ocr_numbers_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ocr_numbers_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ocr_numbers_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ocr_numbers_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ocr_numbers_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
