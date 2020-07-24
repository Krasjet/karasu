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

import Control.Monad           (when)
import Control.Monad.Except    (MonadError)
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Reader    (asks)
import Data.Aeson
import Data.Text               (Text)
import Database.Persist.Sqlite
import GHC.Generics
import Libkst.Hash
import Libkst.IO
import Servant
import System.FilePath         ((<.>), (</>))

data SaveDocBody
   = SaveDocBody
   { docId :: DocId
   , markdown :: Markdown
   , version :: Version
     -- ^ version on the client side, we need to check for version conflicts
   , accessCode :: Maybe AccessCode
   }
  deriving (Generic, Show)

data SaveDocRes
   = SaveDocRes
   { newVersion :: Version
   , html :: Text
   }
  deriving (Generic, Show)

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
  --  an *almost* unique identifier for the markdown file
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
saveDoc reqBody = do
  let dId = docId reqBody
  let clientVer = version reqBody
  let md = markdown reqBody

  -- first validate access code
  (docKey, doc) <- getDocWithValidation dId (accessCode reqBody)

  -- back up markdown file in all cases
  let currVer = docInfoVersion doc
  liftIO $ backupMarkdown dId md (clientVer + 1)

  -- ideally can't happen
  when (clientVer > currVer) $
    throwError err400 { errBody = "Stop messing around with the server." }
  -- if the client version is outdated, don't update.
  when (clientVer < currVer) throwNewVerAvailable

  -- regenerate html
  tmpl <- asks envTemplate
  h <- regenPreview dId tmpl md

  -- update the document (atomic)
  res <- runDb $ updateGet docKey
    [ DocInfoVersion +=. 1
    , DocInfoText =. md
    , DocInfoRenderedHtml =. h
    ]
  return $ SaveDocRes (docInfoVersion res) h
