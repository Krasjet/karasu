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
import Karasu.Pandoc

import qualified Data.Text as T

import Control.Monad           (unless, when, void)
import Control.Monad.Reader    (asks)
import Data.Aeson
import Data.Char               (isAlphaNum)
import Data.Maybe              (isNothing)
import Data.Text               (Text)
import Database.Persist.Sqlite
import GHC.Generics
import Servant
import System.FilePath         (isPathSeparator, isValid)

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

-- | Validate the docId, since we later need to back it up as file
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
    throwError err400 { errBody = "Can you read the document id? I can't." }

  -- finally insert the document
  let defaultTxt = T.pack $ "---\ntitle: " <> dId <> "\n---\n\n"
  res <- runDb $ insertUnique $ DocInfo dId (accessCode docBody) 1 defaultTxt

  -- the docId already exist
  when (isNothing res) $
    throwError err409 { errBody = "Something is already there." }

  -- save the default markdown preview to file
  tmpl <- asks envTemplate
  void $ renderSaveMarkdownPreview dId tmpl defaultTxt

  return "The doc is up. Hooray!"
