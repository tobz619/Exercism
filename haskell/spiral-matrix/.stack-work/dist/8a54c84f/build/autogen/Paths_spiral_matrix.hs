{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_spiral_matrix (
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
version = Version [1,1,0,3] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\Tobi\\Exercism\\haskell\\spiral-matrix\\.stack-work\\install\\7eb224f8\\bin"
libdir     = "C:\\Users\\Tobi\\Exercism\\haskell\\spiral-matrix\\.stack-work\\install\\7eb224f8\\lib\\x86_64-windows-ghc-9.2.7\\spiral-matrix-1.1.0.3-KZhhxKx3QMu87AZdnWsZum"
dynlibdir  = "C:\\Users\\Tobi\\Exercism\\haskell\\spiral-matrix\\.stack-work\\install\\7eb224f8\\lib\\x86_64-windows-ghc-9.2.7"
datadir    = "C:\\Users\\Tobi\\Exercism\\haskell\\spiral-matrix\\.stack-work\\install\\7eb224f8\\share\\x86_64-windows-ghc-9.2.7\\spiral-matrix-1.1.0.3"
libexecdir = "C:\\Users\\Tobi\\Exercism\\haskell\\spiral-matrix\\.stack-work\\install\\7eb224f8\\libexec\\x86_64-windows-ghc-9.2.7\\spiral-matrix-1.1.0.3"
sysconfdir = "C:\\Users\\Tobi\\Exercism\\haskell\\spiral-matrix\\.stack-work\\install\\7eb224f8\\etc"

getBinDir     = catchIO (getEnv "spiral_matrix_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "spiral_matrix_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "spiral_matrix_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "spiral_matrix_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "spiral_matrix_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "spiral_matrix_sysconfdir") (\_ -> return sysconfdir)




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