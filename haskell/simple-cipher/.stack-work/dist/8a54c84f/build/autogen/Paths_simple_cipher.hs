{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_simple_cipher (
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
version = Version [1,2,0,5] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\Tobi\\Exercism\\haskell\\simple-cipher\\.stack-work\\install\\7eb224f8\\bin"
libdir     = "C:\\Users\\Tobi\\Exercism\\haskell\\simple-cipher\\.stack-work\\install\\7eb224f8\\lib\\x86_64-windows-ghc-9.2.7\\simple-cipher-1.2.0.5-DiITPGTjUCLvhlxZ7VuOJ"
dynlibdir  = "C:\\Users\\Tobi\\Exercism\\haskell\\simple-cipher\\.stack-work\\install\\7eb224f8\\lib\\x86_64-windows-ghc-9.2.7"
datadir    = "C:\\Users\\Tobi\\Exercism\\haskell\\simple-cipher\\.stack-work\\install\\7eb224f8\\share\\x86_64-windows-ghc-9.2.7\\simple-cipher-1.2.0.5"
libexecdir = "C:\\Users\\Tobi\\Exercism\\haskell\\simple-cipher\\.stack-work\\install\\7eb224f8\\libexec\\x86_64-windows-ghc-9.2.7\\simple-cipher-1.2.0.5"
sysconfdir = "C:\\Users\\Tobi\\Exercism\\haskell\\simple-cipher\\.stack-work\\install\\7eb224f8\\etc"

getBinDir     = catchIO (getEnv "simple_cipher_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "simple_cipher_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "simple_cipher_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "simple_cipher_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "simple_cipher_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "simple_cipher_sysconfdir") (\_ -> return sysconfdir)




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
