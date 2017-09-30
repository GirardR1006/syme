{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ProjetSyme (
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

bindir     = "/home/rhapsodos/.cabal/bin"
libdir     = "/home/rhapsodos/.cabal/lib/x86_64-linux-ghc-8.2.1/ProjetSyme-0.1.0.0-3fLc5A9DM05Hp0K4B9g6o3"
dynlibdir  = "/home/rhapsodos/.cabal/lib/x86_64-linux-ghc-8.2.1"
datadir    = "/home/rhapsodos/.cabal/share/x86_64-linux-ghc-8.2.1/ProjetSyme-0.1.0.0"
libexecdir = "/home/rhapsodos/.cabal/libexec/x86_64-linux-ghc-8.2.1/ProjetSyme-0.1.0.0"
sysconfdir = "/home/rhapsodos/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ProjetSyme_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ProjetSyme_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ProjetSyme_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ProjetSyme_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ProjetSyme_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ProjetSyme_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
