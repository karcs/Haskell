module Paths_xmonad (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,12], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/jakob/.cabal/bin"
libdir     = "/home/jakob/.cabal/lib/x86_64-linux-ghc-7.6.3/xmonad-0.12"
datadir    = "/home/jakob/.cabal/share/x86_64-linux-ghc-7.6.3/xmonad-0.12"
libexecdir = "/home/jakob/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "xmonad_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "xmonad_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "xmonad_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xmonad_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
