{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_resistor_color_trio (
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
version = Version [1,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Tobi\\Exercism\\haskell\\resistor-color-trio\\.stack-work\\install\\b18ebbf0\\bin"
libdir     = "C:\\Users\\Tobi\\Exercism\\haskell\\resistor-color-trio\\.stack-work\\install\\b18ebbf0\\lib\\x86_64-windows-ghc-9.0.2\\resistor-color-trio-1.0.0.0-1AEwZpd2KEC1G7e2InVtKI"
dynlibdir  = "C:\\Users\\Tobi\\Exercism\\haskell\\resistor-color-trio\\.stack-work\\install\\b18ebbf0\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "C:\\Users\\Tobi\\Exercism\\haskell\\resistor-color-trio\\.stack-work\\install\\b18ebbf0\\share\\x86_64-windows-ghc-9.0.2\\resistor-color-trio-1.0.0.0"
libexecdir = "C:\\Users\\Tobi\\Exercism\\haskell\\resistor-color-trio\\.stack-work\\install\\b18ebbf0\\libexec\\x86_64-windows-ghc-9.0.2\\resistor-color-trio-1.0.0.0"
sysconfdir = "C:\\Users\\Tobi\\Exercism\\haskell\\resistor-color-trio\\.stack-work\\install\\b18ebbf0\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "resistor_color_trio_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "resistor_color_trio_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "resistor_color_trio_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "resistor_color_trio_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "resistor_color_trio_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "resistor_color_trio_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
