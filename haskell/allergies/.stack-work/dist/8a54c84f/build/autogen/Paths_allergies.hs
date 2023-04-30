{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_allergies (
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
version = Version [1,2,0,7] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\Tobi\\Exercism\\haskell\\allergies\\.stack-work\\install\\7eb224f8\\bin"
libdir     = "C:\\Users\\Tobi\\Exercism\\haskell\\allergies\\.stack-work\\install\\7eb224f8\\lib\\x86_64-windows-ghc-9.2.7\\allergies-1.2.0.7-Bziq5oyVWGU1mC3zkULctc"
dynlibdir  = "C:\\Users\\Tobi\\Exercism\\haskell\\allergies\\.stack-work\\install\\7eb224f8\\lib\\x86_64-windows-ghc-9.2.7"
datadir    = "C:\\Users\\Tobi\\Exercism\\haskell\\allergies\\.stack-work\\install\\7eb224f8\\share\\x86_64-windows-ghc-9.2.7\\allergies-1.2.0.7"
libexecdir = "C:\\Users\\Tobi\\Exercism\\haskell\\allergies\\.stack-work\\install\\7eb224f8\\libexec\\x86_64-windows-ghc-9.2.7\\allergies-1.2.0.7"
sysconfdir = "C:\\Users\\Tobi\\Exercism\\haskell\\allergies\\.stack-work\\install\\7eb224f8\\etc"

getBinDir     = catchIO (getEnv "allergies_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "allergies_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "allergies_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "allergies_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "allergies_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "allergies_sysconfdir") (\_ -> return sysconfdir)




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