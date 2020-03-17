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
  -- we need first check the existence of document
  (Entity _ doc) <- getDoc404WithMsg docId "Nothing here."

  -- also send the version string to let kamome know if we have updated
  let docVer = docInfoVersion doc

  -- read the markdown file (NOTE change to database storage?)
  docDir <- asks envDocDir
  let mdFile = docDir </> docId <.> ".md"
  md <- liftIO $ TIO.readFile mdFile

  return $ GetDocRes md docVer
