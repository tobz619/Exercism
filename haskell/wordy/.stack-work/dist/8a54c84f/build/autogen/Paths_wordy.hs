{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_wordy (
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
version = Version [1,5,0,8] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\Tobi\\Exercism\\haskell\\wordy\\.stack-work\\install\\41ab5eec\\bin"
libdir     = "C:\\Users\\Tobi\\Exercism\\haskell\\wordy\\.stack-work\\install\\41ab5eec\\lib\\x86_64-windows-ghc-9.2.5\\wordy-1.5.0.8-Ab1c1NPaCDN6sSi2QgLT3b"
dynlibdir  = "C:\\Users\\Tobi\\Exercism\\haskell\\wordy\\.stack-work\\install\\41ab5eec\\lib\\x86_64-windows-ghc-9.2.5"
datadir    = "C:\\Users\\Tobi\\Exercism\\haskell\\wordy\\.stack-work\\install\\41ab5eec\\share\\x86_64-windows-ghc-9.2.5\\wordy-1.5.0.8"
libexecdir = "C:\\Users\\Tobi\\Exercism\\haskell\\wordy\\.stack-work\\install\\41ab5eec\\libexec\\x86_64-windows-ghc-9.2.5\\wordy-1.5.0.8"
sysconfdir = "C:\\Users\\Tobi\\Exercism\\haskell\\wordy\\.stack-work\\install\\41ab5eec\\etc"

getBinDir     = catchIO (getEnv "wordy_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "wordy_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "wordy_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "wordy_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wordy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "wordy_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
