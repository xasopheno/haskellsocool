{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_haskellsocool (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/danny/code/haskellsocool/.stack-work/install/x86_64-linux-tinfo6/603dfc51ac51bc55602d44ebf96ecd9ab6996beae1b35fd50acc7224e8d83c3f/8.10.4/bin"
libdir     = "/home/danny/code/haskellsocool/.stack-work/install/x86_64-linux-tinfo6/603dfc51ac51bc55602d44ebf96ecd9ab6996beae1b35fd50acc7224e8d83c3f/8.10.4/lib/x86_64-linux-ghc-8.10.4/haskellsocool-0.0.0-BMsLrAeL3AQ1AUj9jatpXH-main"
dynlibdir  = "/home/danny/code/haskellsocool/.stack-work/install/x86_64-linux-tinfo6/603dfc51ac51bc55602d44ebf96ecd9ab6996beae1b35fd50acc7224e8d83c3f/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/danny/code/haskellsocool/.stack-work/install/x86_64-linux-tinfo6/603dfc51ac51bc55602d44ebf96ecd9ab6996beae1b35fd50acc7224e8d83c3f/8.10.4/share/x86_64-linux-ghc-8.10.4/haskellsocool-0.0.0"
libexecdir = "/home/danny/code/haskellsocool/.stack-work/install/x86_64-linux-tinfo6/603dfc51ac51bc55602d44ebf96ecd9ab6996beae1b35fd50acc7224e8d83c3f/8.10.4/libexec/x86_64-linux-ghc-8.10.4/haskellsocool-0.0.0"
sysconfdir = "/home/danny/code/haskellsocool/.stack-work/install/x86_64-linux-tinfo6/603dfc51ac51bc55602d44ebf96ecd9ab6996beae1b35fd50acc7224e8d83c3f/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskellsocool_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskellsocool_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskellsocool_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskellsocool_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskellsocool_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskellsocool_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
