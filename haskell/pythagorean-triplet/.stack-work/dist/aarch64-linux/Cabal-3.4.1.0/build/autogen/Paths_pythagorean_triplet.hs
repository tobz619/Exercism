{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_pythagorean_triplet (
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
version = Version [1,0,0,4] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/tobzdaman619/Exercism/haskell/pythagorean-triplet/.stack-work/install/aarch64-linux/7fb5814d3f9b9dba44a9d275acfd18b5183caa952b67f3fff7d9712dada3ffee/9.0.2/bin"
libdir     = "/home/tobzdaman619/Exercism/haskell/pythagorean-triplet/.stack-work/install/aarch64-linux/7fb5814d3f9b9dba44a9d275acfd18b5183caa952b67f3fff7d9712dada3ffee/9.0.2/lib/aarch64-linux-ghc-9.0.2/pythagorean-triplet-1.0.0.4-3cPgYKMROhUIAWeqhFwEqu"
dynlibdir  = "/home/tobzdaman619/Exercism/haskell/pythagorean-triplet/.stack-work/install/aarch64-linux/7fb5814d3f9b9dba44a9d275acfd18b5183caa952b67f3fff7d9712dada3ffee/9.0.2/lib/aarch64-linux-ghc-9.0.2"
datadir    = "/home/tobzdaman619/Exercism/haskell/pythagorean-triplet/.stack-work/install/aarch64-linux/7fb5814d3f9b9dba44a9d275acfd18b5183caa952b67f3fff7d9712dada3ffee/9.0.2/share/aarch64-linux-ghc-9.0.2/pythagorean-triplet-1.0.0.4"
libexecdir = "/home/tobzdaman619/Exercism/haskell/pythagorean-triplet/.stack-work/install/aarch64-linux/7fb5814d3f9b9dba44a9d275acfd18b5183caa952b67f3fff7d9712dada3ffee/9.0.2/libexec/aarch64-linux-ghc-9.0.2/pythagorean-triplet-1.0.0.4"
sysconfdir = "/home/tobzdaman619/Exercism/haskell/pythagorean-triplet/.stack-work/install/aarch64-linux/7fb5814d3f9b9dba44a9d275acfd18b5183caa952b67f3fff7d9712dada3ffee/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "pythagorean_triplet_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "pythagorean_triplet_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "pythagorean_triplet_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "pythagorean_triplet_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pythagorean_triplet_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pythagorean_triplet_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
