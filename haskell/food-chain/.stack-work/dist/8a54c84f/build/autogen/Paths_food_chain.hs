{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_food_chain (
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
version = Version [0,1,0,3] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\Tobi\\Exercism\\haskell\\food-chain\\.stack-work\\install\\7eb224f8\\bin"
libdir     = "C:\\Users\\Tobi\\Exercism\\haskell\\food-chain\\.stack-work\\install\\7eb224f8\\lib\\x86_64-windows-ghc-9.2.7\\food-chain-0.1.0.3-9WUJsl25gvpKAD1Gy1rwRP"
dynlibdir  = "C:\\Users\\Tobi\\Exercism\\haskell\\food-chain\\.stack-work\\install\\7eb224f8\\lib\\x86_64-windows-ghc-9.2.7"
datadir    = "C:\\Users\\Tobi\\Exercism\\haskell\\food-chain\\.stack-work\\install\\7eb224f8\\share\\x86_64-windows-ghc-9.2.7\\food-chain-0.1.0.3"
libexecdir = "C:\\Users\\Tobi\\Exercism\\haskell\\food-chain\\.stack-work\\install\\7eb224f8\\libexec\\x86_64-windows-ghc-9.2.7\\food-chain-0.1.0.3"
sysconfdir = "C:\\Users\\Tobi\\Exercism\\haskell\\food-chain\\.stack-work\\install\\7eb224f8\\etc"

getBinDir     = catchIO (getEnv "food_chain_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "food_chain_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "food_chain_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "food_chain_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "food_chain_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "food_chain_sysconfdir") (\_ -> return sysconfdir)




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