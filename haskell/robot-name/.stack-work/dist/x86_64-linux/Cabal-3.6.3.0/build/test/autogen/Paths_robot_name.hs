{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_robot_name (
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
version = Version [0,1,0,4] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/tobzdaman619/Exercism/haskell/robot-name/.stack-work/install/x86_64-linux/7579d3deb0f08a55cf6808c930775f7cb0dc1d0d5b8c32983c8cea97c5d75845/9.2.7/bin"
libdir     = "/home/tobzdaman619/Exercism/haskell/robot-name/.stack-work/install/x86_64-linux/7579d3deb0f08a55cf6808c930775f7cb0dc1d0d5b8c32983c8cea97c5d75845/9.2.7/lib/x86_64-linux-ghc-9.2.7/robot-name-0.1.0.4-BSpTTrAhWXg8cpDCn6z1d3-test"
dynlibdir  = "/home/tobzdaman619/Exercism/haskell/robot-name/.stack-work/install/x86_64-linux/7579d3deb0f08a55cf6808c930775f7cb0dc1d0d5b8c32983c8cea97c5d75845/9.2.7/lib/x86_64-linux-ghc-9.2.7"
datadir    = "/home/tobzdaman619/Exercism/haskell/robot-name/.stack-work/install/x86_64-linux/7579d3deb0f08a55cf6808c930775f7cb0dc1d0d5b8c32983c8cea97c5d75845/9.2.7/share/x86_64-linux-ghc-9.2.7/robot-name-0.1.0.4"
libexecdir = "/home/tobzdaman619/Exercism/haskell/robot-name/.stack-work/install/x86_64-linux/7579d3deb0f08a55cf6808c930775f7cb0dc1d0d5b8c32983c8cea97c5d75845/9.2.7/libexec/x86_64-linux-ghc-9.2.7/robot-name-0.1.0.4"
sysconfdir = "/home/tobzdaman619/Exercism/haskell/robot-name/.stack-work/install/x86_64-linux/7579d3deb0f08a55cf6808c930775f7cb0dc1d0d5b8c32983c8cea97c5d75845/9.2.7/etc"

getBinDir     = catchIO (getEnv "robot_name_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "robot_name_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "robot_name_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "robot_name_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "robot_name_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "robot_name_sysconfdir") (\_ -> return sysconfdir)




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
