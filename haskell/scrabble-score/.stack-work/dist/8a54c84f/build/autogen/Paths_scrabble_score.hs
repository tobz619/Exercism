{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_scrabble_score (
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
version = Version [1,1,0,5] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\Tobi\\Exercism\\haskell\\scrabble-score\\.stack-work\\install\\9fc3ba5e\\bin"
libdir     = "C:\\Users\\Tobi\\Exercism\\haskell\\scrabble-score\\.stack-work\\install\\9fc3ba5e\\lib\\x86_64-windows-ghc-9.2.5\\scrabble-score-1.1.0.5-2GoH3bxdyoVCOdvS0l8zCB"
dynlibdir  = "C:\\Users\\Tobi\\Exercism\\haskell\\scrabble-score\\.stack-work\\install\\9fc3ba5e\\lib\\x86_64-windows-ghc-9.2.5"
datadir    = "C:\\Users\\Tobi\\Exercism\\haskell\\scrabble-score\\.stack-work\\install\\9fc3ba5e\\share\\x86_64-windows-ghc-9.2.5\\scrabble-score-1.1.0.5"
libexecdir = "C:\\Users\\Tobi\\Exercism\\haskell\\scrabble-score\\.stack-work\\install\\9fc3ba5e\\libexec\\x86_64-windows-ghc-9.2.5\\scrabble-score-1.1.0.5"
sysconfdir = "C:\\Users\\Tobi\\Exercism\\haskell\\scrabble-score\\.stack-work\\install\\9fc3ba5e\\etc"

getBinDir     = catchIO (getEnv "scrabble_score_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "scrabble_score_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "scrabble_score_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "scrabble_score_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "scrabble_score_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "scrabble_score_sysconfdir") (\_ -> return sysconfdir)




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
