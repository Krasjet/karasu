-- Heavily modified from https://github.com/phadej/latex-svg
-- 2020 Krasjet, 2020 Oleg Grenrus, 2015-2019 Liam O'Connor

-- | Utility functions for the LaTeX renderer
module Karasu.Pandoc.Filters.LaTeX.Utils (
  spanL,
  spanR,
  readProcessWithCWD,
  tryIO,
  hashText,
  hashText',
  createParentDir,
  writeFileHandleMissing,
  writeFileHandleMissing',
  writeFileHandleMissingS
) where

import qualified Control.Exception as E
import qualified System.Process    as Proc

import qualified Crypto.Hash.SHA256      as SHA256
import qualified Data.ByteString.Base64  as Base64
import qualified Data.ByteString.Char8   as BS8
import qualified Data.Text               as T
import qualified Data.Text.IO            as TIO
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO       as LTIO

import Control.DeepSeq            (NFData (..), ($!!))
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Data.List                  (isPrefixOf)
import System.Directory           (createDirectoryIfMissing)
import System.Exit                (ExitCode (..))
import System.FilePath            (takeDirectory)

-- * Parsing

-- | Search for the first instanse of sep, for example,
-- spanL "<xml" "<p><xml" == ("<p>","<xml")
spanL :: Eq a => [a] -> [a] -> ([a], [a])
spanL sep = go where
  go str@[]                  = (str, str)
  go str@(c:sfx)
    | sep `isPrefixOf` str = ([], str)
    | otherwise            = (c:xs , ys)
    where
      ~(xs,ys) = go sfx

-- | search for the first instanse of sep, e.g.
-- spanR '>' "<xml></p>" == ("<xml>","</p>")
spanR :: Eq a => a -> [a] -> ([a], [a])
spanR sep = go where
  go  str@[]      = (str, str)
  go _str@(c:sfx)
    | sep == c  = ([c], sfx)
    | otherwise = (c:xs , ys)
    where
      ~(xs,ys) = go sfx

-- * System process/IO

-- | Fork, execute the process and return the result
readProcessWithCWD
  :: FilePath                      -- ^ current working directory
  -> FilePath                      -- ^ filename of the executable
  -> [String]                      -- ^ arguments
  -> IO (ExitCode, String, String) -- ^ exitcode, stdout, stderr
readProcessWithCWD cwd cmd args = Proc.readCreateProcessWithExitCode
  ((Proc.proc cmd args) { Proc.cwd = Just cwd }) ""
  --                                             ^ no stdin

-- | Catch 'IOException's and convert them to the 'ExceptT' monad
tryIO :: (MonadIO m, NFData a) => IO a -> ExceptT E.IOException m a
tryIO action = ExceptT $ liftIO $ E.try $ evaluateDeep action
  where
    evaluateDeep :: NFData a => IO a -> IO a
    evaluateDeep act = do
      res <- act
      E.evaluate $!! res

-- | Create parent directory for a file
createParentDir
  :: FilePath -- ^ filename containing path
  -> IO ()
createParentDir = createDirectoryIfMissing True . takeDirectory

-- | Write text to a file with missing directory handling
writeFileHandleMissing
  :: FilePath -- ^ filename containing path
  -> LT.Text  -- ^ string to be written
  -> IO ()
writeFileHandleMissing f t = do
  createParentDir f
  LTIO.writeFile f t

-- | The strict version of writeFileHandleMissing
writeFileHandleMissing'
  :: FilePath -- ^ filename containing path
  -> T.Text   -- ^ string to be written
  -> IO ()
writeFileHandleMissing' f t = do
  createParentDir f
  TIO.writeFile f t

-- | The String version of writeFileHandleMissing
writeFileHandleMissingS
  :: FilePath -- ^ filename containing path
  -> String   -- ^ string to be written
  -> IO ()
writeFileHandleMissingS f t = do
  createParentDir f
  writeFile f t

-- * Hashing related

-- | Create an *almost* unique identifier for a text string,
-- which is the length of the text plus its SHA256 hash
-- The chance of collision should be ignorable
hashText :: LT.Text -> String
hashText text =
  show len <> filter validHash (BS8.unpack $ Base64.encode t)
    where
      (t, len) = SHA256.hashlazyAndLength $ LT.encodeUtf8 text
      validHash :: Char -> Bool
      validHash c = c /= '=' && c /= '/'

-- | The strict version of hash text
hashText' :: T.Text -> String
hashText' = hashText . LT.fromStrict
