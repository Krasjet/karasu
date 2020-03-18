{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | API saving documents
module Karasu.Handlers.ApiSaveDoc (SaveDocApi, saveDoc) where

import Karasu.Database
import Karasu.Handler
import Karasu.Models
import Karasu.Pandoc.Renderer
import Karasu.Utils

import qualified Crypto.Hash.SHA1           as SHA1
import qualified Data.ByteString.Base16     as Base16
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Text                  as T

import Control.Monad           (when)
import Control.Monad.Except    (MonadError)
import Control.Monad.IO.Class  (liftIO)
import Data.Aeson
import Data.Text               (Text)
import Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import Database.Persist.Sqlite
import GHC.Generics
import Servant
import System.FilePath         ((<.>), (</>))

data SaveDocBody = SaveDocBody {
  docId      :: DocId,
  markdown   :: Markdown,
  version    :: Version, -- version on the client side, we need to check for version conflicts
  accessCode :: Maybe AccessCode
} deriving (Generic, Show)

data SaveDocRes = SaveDocRes {
  newVersion :: Version,
  html       :: Text
} deriving (Generic, Show)

instance ToJSON   SaveDocBody
instance FromJSON SaveDocBody
instance ToJSON   SaveDocRes
instance FromJSON SaveDocRes

type SaveDocApi = "api"
               :> "save"
               :> ReqBody '[JSON] SaveDocBody
               :> Post '[JSON] SaveDocRes

-- | Backup markdown file with version number and hashed file name
backupMarkdown
  :: DocId    -- ^ document ID
  -> Markdown -- ^ markdown text
  -> Version  -- ^ client side version + 1
  -> IO ()
backupMarkdown dId md ver = do
  -- unique identifier for the markdown file
  let hash = Base16.encode $ SHA1.hash $ encodeUtf8 md
  let filename = "backup" </> dId </> show ver <> "-" <> T.unpack (decodeUtf8 hash) <.> "md"
  writeFileHandleMissing filename md

throwNewVerAvailable :: (MonadError ServerError m) => m ()
throwNewVerAvailable = throwError err409 { errBody = "A newer version of the document is \
    \available. The document is backed up on the server but not saved. Refresh \
    \to retrieve the latest version" }

-- | Save the document to file
saveDoc :: SaveDocBody -> KHandler SaveDocRes
saveDoc saveBody = do
  let dId = docId saveBody
  let clientVer = version saveBody
  let md = markdown saveBody

  -- first validate access code
  (docKey, doc) <- getDocWithValidation dId (accessCode saveBody)

  -- back up markdown file in all cases
  let currVer = docInfoVersion doc
  liftIO $ backupMarkdown dId md (clientVer + 1)

  -- ideally can't happen
  when (clientVer > currVer) $
    throwError err400 { errBody = "Stop messing around with the server." }
  -- if the client version is outdated, don't update.
  when (clientVer < currVer) throwNewVerAvailable

  -- TODO protect with MVar or TVar, since index.html and database needs to be synced

  -- actually update the document (atomic)
  res <- runDb $ updateGet docKey [ DocInfoVersion +=. 1, DocInfoText =. md ]

  -- now, start rendering the markdown file
  out <- liftIO $ renderPreviewText md
  case out of
    Left err -> throwError err400 { errBody = LB8.pack $ show err }
    Right h  -> do
      let htmlFile = "view" </> dId </> "index" <.> "html"
      liftIO $ writeFileHandleMissing htmlFile h
      return $ SaveDocRes (docInfoVersion res) h
  -- TODO protect with MVar/TVar
