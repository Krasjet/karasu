{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | API for retrieving the markdown file
module Karasu.Handlers.ApiGetDoc (GetDocApi, getDoc) where

import Karasu.Database
import Karasu.Handler
import Karasu.Models

import Data.Aeson
import GHC.Generics
import Servant

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
  (_, doc) <- getDoc404WithMsg docId "Nothing here."

  -- also send the version string to let kamome know if we have updated
  let docVer = docInfoVersion doc
  -- read the markdown file
  let md = docInfoText doc

  return $ GetDocRes md docVer
