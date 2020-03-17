-- | Miscellaneous utility functions
module Karasu.Utils (createParentDir, writeFileHandleMissing) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import System.Directory (createDirectoryIfMissing)
import System.FilePath  (takeDirectory)

-- | Create parent directory for a file
createParentDir
  :: FilePath -- ^ filename containing path
  -> IO ()
createParentDir = createDirectoryIfMissing True . takeDirectory

-- | Write string to a file with missing directory handling
writeFileHandleMissing
  :: FilePath -- ^ filename containing path
  -> T.Text   -- ^ string to be written
  -> IO ()
writeFileHandleMissing f t = do
  createParentDir f
  TIO.writeFile f t
