{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_propagators2 (
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
version = Version [0,1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/arved/Documents/Swansea Experiments/propagators2/.stack-work/install/x86_64-linux/a0ddd5a1b519f9fe13abb06f52ef881e3a8259d7921745b4a6023546188085d0/8.10.3/bin"
libdir     = "/home/arved/Documents/Swansea Experiments/propagators2/.stack-work/install/x86_64-linux/a0ddd5a1b519f9fe13abb06f52ef881e3a8259d7921745b4a6023546188085d0/8.10.3/lib/x86_64-linux-ghc-8.10.3/propagators2-0.1.0-1SqAmZ35nIpxwGHS1VInI-propagators2"
dynlibdir  = "/home/arved/Documents/Swansea Experiments/propagators2/.stack-work/install/x86_64-linux/a0ddd5a1b519f9fe13abb06f52ef881e3a8259d7921745b4a6023546188085d0/8.10.3/lib/x86_64-linux-ghc-8.10.3"
datadir    = "/home/arved/Documents/Swansea Experiments/propagators2/.stack-work/install/x86_64-linux/a0ddd5a1b519f9fe13abb06f52ef881e3a8259d7921745b4a6023546188085d0/8.10.3/share/x86_64-linux-ghc-8.10.3/propagators2-0.1.0"
libexecdir = "/home/arved/Documents/Swansea Experiments/propagators2/.stack-work/install/x86_64-linux/a0ddd5a1b519f9fe13abb06f52ef881e3a8259d7921745b4a6023546188085d0/8.10.3/libexec/x86_64-linux-ghc-8.10.3/propagators2-0.1.0"
sysconfdir = "/home/arved/Documents/Swansea Experiments/propagators2/.stack-work/install/x86_64-linux/a0ddd5a1b519f9fe13abb06f52ef881e3a8259d7921745b4a6023546188085d0/8.10.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "propagators2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "propagators2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "propagators2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "propagators2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "propagators2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "propagators2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
