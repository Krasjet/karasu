{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | API for creating a document, for private use only
module Karasu.Handlers.ApiCreateDoc (CreateDocApi, createDoc) where

import Karasu.Database
import Karasu.Environment
import Karasu.Handler
import Karasu.Models

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import Control.Monad           (unless, when)
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Reader    (asks)
import Data.Aeson
import Data.Char               (isAlphaNum)
import Data.Maybe              (isNothing)
import Data.Text               (Text)
import Database.Persist.Sqlite
import GHC.Generics
import Servant
import System.Directory        (createDirectoryIfMissing)
import System.FilePath         (isPathSeparator, isValid, takeDirectory, (<.>),
                                (</>))

data CreateDocBody = CreateDocBody {
  docId      :: DocId,
  accessCode :: Maybe AccessCode,
  masterPass :: MasterPassword
} deriving (Generic, Show)

instance ToJSON CreateDocBody
instance FromJSON CreateDocBody

type CreateDocApi = "api"
                 :> "create"
                 :> ReqBody '[JSON] CreateDocBody
                 :> PostCreated '[PlainText] Text

-- | Validate the docId
-- 1. valid path (empty string should fail here)
-- 2. not containing any path separator
-- 3. start with alphanumeric
validId :: FilePath -> Bool
validId path = isValid path && not (any isPathSeparator path) && isAlphaNum (head path)

-- | Create new document
createDoc :: CreateDocBody -> KHandler Text
createDoc docBody = do
  correctPass <- asks envMaster
  -- master password incorrect
  unless (correctPass == masterPass docBody) $
    throwError err403 { errBody = "Nope, try again." }

  let dId = docId docBody
  -- docId not valid filename
  unless (validId dId) $
    throwError err400 { errBody = "Can you read the document id?" }

  -- finally insert the document
  res <- runDb $ insertUnique $ DocInfo (docId docBody) (accessCode docBody) 1
  -- the docId already exists
  when (isNothing res) $
    throwError err409 { errBody = "Something is already there." }

  -- write default markdown file
  docDir <- asks envDocDir
  let mdFile = docDir </> dId <.> ".md"
  liftIO $ createDirectoryIfMissing True $ takeDirectory mdFile
  liftIO $ TIO.writeFile mdFile $ T.pack $ "---\ntitle: " <> dId <> "\n---"
  return "The doc is up. Hooray!"
