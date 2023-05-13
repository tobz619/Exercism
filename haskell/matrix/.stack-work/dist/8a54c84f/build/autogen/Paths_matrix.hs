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
bindir     = "C:\\Users\\Tobi\\Exercism\\haskell\\matrix\\.stack-work\\install\\dc607eb2\\bin"
libdir     = "C:\\Users\\Tobi\\Exercism\\haskell\\matrix\\.stack-work\\install\\dc607eb2\\lib\\x86_64-windows-ghc-9.2.7\\matrix-1.3.0.9-8eCmuzDgk4UIIJ3ndO1JVB"
dynlibdir  = "C:\\Users\\Tobi\\Exercism\\haskell\\matrix\\.stack-work\\install\\dc607eb2\\lib\\x86_64-windows-ghc-9.2.7"
datadir    = "C:\\Users\\Tobi\\Exercism\\haskell\\matrix\\.stack-work\\install\\dc607eb2\\share\\x86_64-windows-ghc-9.2.7\\matrix-1.3.0.9"
libexecdir = "C:\\Users\\Tobi\\Exercism\\haskell\\matrix\\.stack-work\\install\\dc607eb2\\libexec\\x86_64-windows-ghc-9.2.7\\matrix-1.3.0.9"
sysconfdir = "C:\\Users\\Tobi\\Exercism\\haskell\\matrix\\.stack-work\\install\\dc607eb2\\etc"

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
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'