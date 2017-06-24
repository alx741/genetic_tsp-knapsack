{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_genetic (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/mnt/hdd/alx/u/optimizacion/genetic/.stack-work/install/x86_64-linux-ncurses6/lts-8.20/8.0.2/bin"
libdir     = "/mnt/hdd/alx/u/optimizacion/genetic/.stack-work/install/x86_64-linux-ncurses6/lts-8.20/8.0.2/lib/x86_64-linux-ghc-8.0.2/genetic-0.1.0.0-4Qteo4SiaPs2YiyYDVTgEQ"
dynlibdir  = "/mnt/hdd/alx/u/optimizacion/genetic/.stack-work/install/x86_64-linux-ncurses6/lts-8.20/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/mnt/hdd/alx/u/optimizacion/genetic/.stack-work/install/x86_64-linux-ncurses6/lts-8.20/8.0.2/share/x86_64-linux-ghc-8.0.2/genetic-0.1.0.0"
libexecdir = "/mnt/hdd/alx/u/optimizacion/genetic/.stack-work/install/x86_64-linux-ncurses6/lts-8.20/8.0.2/libexec"
sysconfdir = "/mnt/hdd/alx/u/optimizacion/genetic/.stack-work/install/x86_64-linux-ncurses6/lts-8.20/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "genetic_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "genetic_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "genetic_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "genetic_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "genetic_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "genetic_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
