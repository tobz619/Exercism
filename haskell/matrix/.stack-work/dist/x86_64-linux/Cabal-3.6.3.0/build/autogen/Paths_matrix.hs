{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_matrix (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [1,3,0,9] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/tobzdaman619/Exercism/haskell/matrix/.stack-work/install/x86_64-linux/6f08483745349a842d6bb2bc42515c0d66c8cfe632b612ec38e207391b86410a/9.2.7/bin"
libdir     = "/home/tobzdaman619/Exercism/haskell/matrix/.stack-work/install/x86_64-linux/6f08483745349a842d6bb2bc42515c0d66c8cfe632b612ec38e207391b86410a/9.2.7/lib/x86_64-linux-ghc-9.2.7/matrix-1.3.0.9-8eCmuzDgk4UIIJ3ndO1JVB"
dynlibdir  = "/home/tobzdaman619/Exercism/haskell/matrix/.stack-work/install/x86_64-linux/6f08483745349a842d6bb2bc42515c0d66c8cfe632b612ec38e207391b86410a/9.2.7/lib/x86_64-linux-ghc-9.2.7"
datadir    = "/home/tobzdaman619/Exercism/haskell/matrix/.stack-work/install/x86_64-linux/6f08483745349a842d6bb2bc42515c0d66c8cfe632b612ec38e207391b86410a/9.2.7/share/x86_64-linux-ghc-9.2.7/matrix-1.3.0.9"
libexecdir = "/home/tobzdaman619/Exercism/haskell/matrix/.stack-work/install/x86_64-linux/6f08483745349a842d6bb2bc42515c0d66c8cfe632b612ec38e207391b86410a/9.2.7/libexec/x86_64-linux-ghc-9.2.7/matrix-1.3.0.9"
sysconfdir = "/home/tobzdaman619/Exercism/haskell/matrix/.stack-work/install/x86_64-linux/6f08483745349a842d6bb2bc42515c0d66c8cfe632b612ec38e207391b86410a/9.2.7/etc"

getBinDir     = catchIO (getEnv "matrix_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "matrix_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "matrix_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "matrix_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "matrix_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "matrix_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
