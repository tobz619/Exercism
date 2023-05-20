{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_palindrome_products (
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
version = Version [1,1,0,4] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\cabal\\bin"
libdir     = "C:\\cabal\\x86_64-windows-ghc-9.2.7\\palindrome-products-1.1.0.4-inplace-test"
dynlibdir  = "C:\\cabal\\x86_64-windows-ghc-9.2.7"
datadir    = "C:\\cabal\\x86_64-windows-ghc-9.2.7\\palindrome-products-1.1.0.4"
libexecdir = "C:\\cabal\\palindrome-products-1.1.0.4-inplace-test\\x86_64-windows-ghc-9.2.7\\palindrome-products-1.1.0.4"
sysconfdir = "C:\\cabal\\etc"

getBinDir     = catchIO (getEnv "palindrome_products_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "palindrome_products_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "palindrome_products_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "palindrome_products_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "palindrome_products_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "palindrome_products_sysconfdir") (\_ -> return sysconfdir)




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
