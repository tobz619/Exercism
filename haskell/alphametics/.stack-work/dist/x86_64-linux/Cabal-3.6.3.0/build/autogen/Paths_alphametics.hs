{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_alphametics (
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
version = Version [1,3,0,6] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/mnt/chromeos/GoogleDrive/MyDrive/Tech-Papers/Programming/Exercism/haskell/alphametics/.stack-work/install/x86_64-linux/5486d4a7f44b8b6b5ec78604639b306fa86887931664d42e94fc765cbcd69da3/9.2.7/bin"
libdir     = "/mnt/chromeos/GoogleDrive/MyDrive/Tech-Papers/Programming/Exercism/haskell/alphametics/.stack-work/install/x86_64-linux/5486d4a7f44b8b6b5ec78604639b306fa86887931664d42e94fc765cbcd69da3/9.2.7/lib/x86_64-linux-ghc-9.2.7/alphametics-1.3.0.6-8gBYvrE6POx3vBkm2enLaq"
dynlibdir  = "/mnt/chromeos/GoogleDrive/MyDrive/Tech-Papers/Programming/Exercism/haskell/alphametics/.stack-work/install/x86_64-linux/5486d4a7f44b8b6b5ec78604639b306fa86887931664d42e94fc765cbcd69da3/9.2.7/lib/x86_64-linux-ghc-9.2.7"
datadir    = "/mnt/chromeos/GoogleDrive/MyDrive/Tech-Papers/Programming/Exercism/haskell/alphametics/.stack-work/install/x86_64-linux/5486d4a7f44b8b6b5ec78604639b306fa86887931664d42e94fc765cbcd69da3/9.2.7/share/x86_64-linux-ghc-9.2.7/alphametics-1.3.0.6"
libexecdir = "/mnt/chromeos/GoogleDrive/MyDrive/Tech-Papers/Programming/Exercism/haskell/alphametics/.stack-work/install/x86_64-linux/5486d4a7f44b8b6b5ec78604639b306fa86887931664d42e94fc765cbcd69da3/9.2.7/libexec/x86_64-linux-ghc-9.2.7/alphametics-1.3.0.6"
sysconfdir = "/mnt/chromeos/GoogleDrive/MyDrive/Tech-Papers/Programming/Exercism/haskell/alphametics/.stack-work/install/x86_64-linux/5486d4a7f44b8b6b5ec78604639b306fa86887931664d42e94fc765cbcd69da3/9.2.7/etc"

getBinDir     = catchIO (getEnv "alphametics_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "alphametics_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "alphametics_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "alphametics_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "alphametics_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "alphametics_sysconfdir") (\_ -> return sysconfdir)




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
