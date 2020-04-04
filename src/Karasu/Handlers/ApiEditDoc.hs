{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | API for creating a document, for private use only
module Karasu.Handlers.ApiEditDoc (EditDocApi, editDoc) where

import Karasu.Database
import Karasu.Handler
import Karasu.Models

import Network.HTTP.Types
import Network.Wai
import Servant
import Servant.RawM
import System.FilePath    ((<.>), (</>))

type EditDocApi = "edit"
               :> Capture "docId" DocId
               :> RawM

editDocApp :: Bool -> Application
editDocApp True _ resp = do
  -- document exists, return editor
  let header = [("Content-Type", "text/html")]
  resp $ responseFile status200 header ("static" </> "editor" <.> "html") Nothing
editDocApp False _ resp =
  -- nothing found, return 404
  resp $ responseLBS status404 [] "Nothing here."

-- | Send the editor page
editDoc :: DocId -> ServerT RawM KHandler
editDoc docId = editDocApp <$> docExists docId
