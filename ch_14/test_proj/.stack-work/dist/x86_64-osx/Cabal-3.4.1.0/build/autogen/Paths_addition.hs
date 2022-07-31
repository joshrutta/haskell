{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_addition (
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

bindir     = "/Users/jrutta/Desktop/personal/haskell_programming_from_first_principals/ch_14/test_proj/.stack-work/install/x86_64-osx/2507f8844d22290d26f626c4bf28b0392d0e41e21bdea649df942e2865fdc922/9.0.2/bin"
libdir     = "/Users/jrutta/Desktop/personal/haskell_programming_from_first_principals/ch_14/test_proj/.stack-work/install/x86_64-osx/2507f8844d22290d26f626c4bf28b0392d0e41e21bdea649df942e2865fdc922/9.0.2/lib/x86_64-osx-ghc-9.0.2/addition-0.1.0.0-6wzEsh5Rda6FZrlvh9j61E"
dynlibdir  = "/Users/jrutta/Desktop/personal/haskell_programming_from_first_principals/ch_14/test_proj/.stack-work/install/x86_64-osx/2507f8844d22290d26f626c4bf28b0392d0e41e21bdea649df942e2865fdc922/9.0.2/lib/x86_64-osx-ghc-9.0.2"
datadir    = "/Users/jrutta/Desktop/personal/haskell_programming_from_first_principals/ch_14/test_proj/.stack-work/install/x86_64-osx/2507f8844d22290d26f626c4bf28b0392d0e41e21bdea649df942e2865fdc922/9.0.2/share/x86_64-osx-ghc-9.0.2/addition-0.1.0.0"
libexecdir = "/Users/jrutta/Desktop/personal/haskell_programming_from_first_principals/ch_14/test_proj/.stack-work/install/x86_64-osx/2507f8844d22290d26f626c4bf28b0392d0e41e21bdea649df942e2865fdc922/9.0.2/libexec/x86_64-osx-ghc-9.0.2/addition-0.1.0.0"
sysconfdir = "/Users/jrutta/Desktop/personal/haskell_programming_from_first_principals/ch_14/test_proj/.stack-work/install/x86_64-osx/2507f8844d22290d26f626c4bf28b0392d0e41e21bdea649df942e2865fdc922/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "addition_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "addition_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "addition_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "addition_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "addition_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "addition_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
