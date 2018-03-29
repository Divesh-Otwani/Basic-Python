{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_brainf (
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

bindir     = "/home/divesh/git_repos/My-Online-Workspace/Haskell/Projects/brainf/.stack-work/install/x86_64-linux/lts-10.4/8.2.2/bin"
libdir     = "/home/divesh/git_repos/My-Online-Workspace/Haskell/Projects/brainf/.stack-work/install/x86_64-linux/lts-10.4/8.2.2/lib/x86_64-linux-ghc-8.2.2/brainf-0.1.0.0-AImJWLrHSqB1DTJtiuLkXj-brainf"
dynlibdir  = "/home/divesh/git_repos/My-Online-Workspace/Haskell/Projects/brainf/.stack-work/install/x86_64-linux/lts-10.4/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/divesh/git_repos/My-Online-Workspace/Haskell/Projects/brainf/.stack-work/install/x86_64-linux/lts-10.4/8.2.2/share/x86_64-linux-ghc-8.2.2/brainf-0.1.0.0"
libexecdir = "/home/divesh/git_repos/My-Online-Workspace/Haskell/Projects/brainf/.stack-work/install/x86_64-linux/lts-10.4/8.2.2/libexec/x86_64-linux-ghc-8.2.2/brainf-0.1.0.0"
sysconfdir = "/home/divesh/git_repos/My-Online-Workspace/Haskell/Projects/brainf/.stack-work/install/x86_64-linux/lts-10.4/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "brainf_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "brainf_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "brainf_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "brainf_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "brainf_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "brainf_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
