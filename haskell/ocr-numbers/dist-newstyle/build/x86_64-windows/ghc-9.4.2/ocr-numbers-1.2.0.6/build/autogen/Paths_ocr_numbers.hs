{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_ocr_numbers (
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
version = Version [1,2,0,6] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\cabal\\bin"
libdir     = "C:\\cabal\\x86_64-windows-ghc-9.4.2\\ocr-numbers-1.2.0.6-inplace"
dynlibdir  = "C:\\cabal\\x86_64-windows-ghc-9.4.2"
datadir    = "C:\\cabal\\x86_64-windows-ghc-9.4.2\\ocr-numbers-1.2.0.6"
libexecdir = "C:\\cabal\\ocr-numbers-1.2.0.6-inplace\\x86_64-windows-ghc-9.4.2\\ocr-numbers-1.2.0.6"
sysconfdir = "C:\\cabal\\etc"

getBinDir     = catchIO (getEnv "ocr_numbers_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "ocr_numbers_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "ocr_numbers_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "ocr_numbers_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ocr_numbers_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ocr_numbers_sysconfdir") (\_ -> return sysconfdir)




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
