{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | API saving documents
module Karasu.Handlers.ApiSaveDoc (SaveDocApi, saveDoc) where

import Karasu.Database
import Karasu.Environment
import Karasu.Handler
import Karasu.Models
import Karasu.Pandoc
import Karasu.Utils

import Control.Monad           (when)
import Control.Monad.Except    (MonadError)
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Reader    (asks)
import Data.Aeson
import Data.Text               (Text)
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
  -- *almost* unique identifier for the markdown file
  let hash = hashText' md
  let filename = "backup" </> dId </> show ver <> "-" <> hash <.> "md"
  writeFileHandleMissing' filename md

throwNewVerAvailable :: (MonadError ServerError m) => m ()
throwNewVerAvailable = throwError err409 { errBody = "A newer version of the\
  \ document is available. The document is backed up on the server but not\
  \ saved. Copy everything in the editor to a file on your own computer and\
  \ refresh to retrieve the latest version.\n\n\
  \ Shoot me an email if you forget to back up your progress." }

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
  -- save the rendered markdown to file
  tmpl <- asks envTemplate
  h <- renderSaveMarkdownPreview dId tmpl md
  return $ SaveDocRes (docInfoVersion res) h
  -- TODO protect with MVar/TVar
