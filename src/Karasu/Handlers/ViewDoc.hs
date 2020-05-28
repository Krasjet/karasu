{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | The route for compiled documents, only for testing. On deployment server,
-- this is handled by nginx.
module Karasu.Handlers.ViewDoc (
  ViewDoc,
  viewDoc
) where

import Karasu.Database
import Karasu.Handler
import Karasu.Models

import Network.HTTP.Types
import Network.Wai
import Servant
import Servant.RawM
import System.FilePath    ((<.>), (</>))

type ViewDoc = "view"
               :> Capture "docId" DocId
               :> RawM

viewDocApp
  :: DocId -- ^ the doc id
  -> Bool  -- ^ docid exists?
  -> Application
viewDocApp docId True _ resp = do
  -- document exists, return preview page
  let header = [("Content-Type", "text/html")]
  resp $ responseFile status200 header ("view" </> docId </> "index" <.> "html") Nothing
viewDocApp _ False _ resp =
  -- nothing found, return 404
  resp $ responseLBS status404 [] "Nothing here."

-- | Send the editor page
viewDoc :: DocId -> ServerT RawM KHandler
viewDoc docId = viewDocApp docId <$> docExists docId
