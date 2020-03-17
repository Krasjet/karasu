{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | API for retrieving the markdown file
module Karasu.Handlers.ApiGetDoc (GetDocApi, getDoc) where

import Karasu.Database
import Karasu.Environment
import Karasu.Handler
import Karasu.Models

import qualified Data.Text.IO as TIO

import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Reader    (asks)
import Data.Aeson
import Database.Persist.Sqlite
import GHC.Generics
import Servant
import System.FilePath         ((<.>), (</>))

data GetDocRes = GetDocRes {
  markdown :: Markdown,
  version  :: Version
} deriving (Generic, Show)

instance ToJSON GetDocRes
instance FromJSON GetDocRes

type GetDocApi = "api"
              :> "get"
              :> Capture "docId" DocId
              :> Get '[JSON] GetDocRes

-- | Return the markdown of the document
getDoc :: DocId -> KHandler GetDocRes
getDoc docId = do
  res <- runDb $ getBy $ UniqueDocId docId
  case res of
    Nothing -> throwError err404 { errBody = "Nothing here." }
    Just (Entity _ doc) -> do
      let docVer = docInfoVersion doc
      docDir <- asks envDocDir
      let mdFile = docDir </> docId <.> ".md"
      md <- liftIO $ TIO.readFile mdFile
      return $ GetDocRes md docVer
