module Paths_my_scheme (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/juan/Programacion/Haskell/my-scheme/.stack-work/install/x86_64-linux/lts-5.16/7.10.3/bin"
libdir     = "/home/juan/Programacion/Haskell/my-scheme/.stack-work/install/x86_64-linux/lts-5.16/7.10.3/lib/x86_64-linux-ghc-7.10.3/my-scheme-0.1.0.0-4s2g40IXkvn3mTo3AYbpWg"
datadir    = "/home/juan/Programacion/Haskell/my-scheme/.stack-work/install/x86_64-linux/lts-5.16/7.10.3/share/x86_64-linux-ghc-7.10.3/my-scheme-0.1.0.0"
libexecdir = "/home/juan/Programacion/Haskell/my-scheme/.stack-work/install/x86_64-linux/lts-5.16/7.10.3/libexec"
sysconfdir = "/home/juan/Programacion/Haskell/my-scheme/.stack-work/install/x86_64-linux/lts-5.16/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "my_scheme_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "my_scheme_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "my_scheme_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "my_scheme_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "my_scheme_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
