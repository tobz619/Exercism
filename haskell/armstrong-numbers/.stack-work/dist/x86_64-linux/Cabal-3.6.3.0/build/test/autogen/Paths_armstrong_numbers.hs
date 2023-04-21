{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_armstrong_numbers (
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
bindir     = "/home/tobzdaman619/Exercism/haskell/armstrong-numbers/.stack-work/install/x86_64-linux/458d2a95f8538a5d8cc40efd48b50f8174ae7e40c4531165c9c89a9c26874bb1/9.2.7/bin"
libdir     = "/home/tobzdaman619/Exercism/haskell/armstrong-numbers/.stack-work/install/x86_64-linux/458d2a95f8538a5d8cc40efd48b50f8174ae7e40c4531165c9c89a9c26874bb1/9.2.7/lib/x86_64-linux-ghc-9.2.7/armstrong-numbers-1.1.0.3-3UtuCLYDU8B7CLsJeC61AO-test"
dynlibdir  = "/home/tobzdaman619/Exercism/haskell/armstrong-numbers/.stack-work/install/x86_64-linux/458d2a95f8538a5d8cc40efd48b50f8174ae7e40c4531165c9c89a9c26874bb1/9.2.7/lib/x86_64-linux-ghc-9.2.7"
datadir    = "/home/tobzdaman619/Exercism/haskell/armstrong-numbers/.stack-work/install/x86_64-linux/458d2a95f8538a5d8cc40efd48b50f8174ae7e40c4531165c9c89a9c26874bb1/9.2.7/share/x86_64-linux-ghc-9.2.7/armstrong-numbers-1.1.0.3"
libexecdir = "/home/tobzdaman619/Exercism/haskell/armstrong-numbers/.stack-work/install/x86_64-linux/458d2a95f8538a5d8cc40efd48b50f8174ae7e40c4531165c9c89a9c26874bb1/9.2.7/libexec/x86_64-linux-ghc-9.2.7/armstrong-numbers-1.1.0.3"
sysconfdir = "/home/tobzdaman619/Exercism/haskell/armstrong-numbers/.stack-work/install/x86_64-linux/458d2a95f8538a5d8cc40efd48b50f8174ae7e40c4531165c9c89a9c26874bb1/9.2.7/etc"

getBinDir     = catchIO (getEnv "armstrong_numbers_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "armstrong_numbers_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "armstrong_numbers_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "armstrong_numbers_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "armstrong_numbers_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "armstrong_numbers_sysconfdir") (\_ -> return sysconfdir)




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
