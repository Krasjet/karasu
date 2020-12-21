{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | The route for rendering compiled documents.
--
-- Note that this route will only be called when document is not up-to-date,
-- normally it would be handled by nginx, when view/docid/index.html is
-- present.
module Karasu.Handlers.ViewDoc (
  ViewDoc,
  viewDoc
) where

import Karasu.Database
import Karasu.Handler
import Karasu.Models
import Chirp.IO
import Chirp.Monad

import Control.Monad.IO.Class  (liftIO)
import Database.Persist.Sqlite
import Network.HTTP.Types
import Network.Wai
import Servant
import Servant.RawM.Server     (RawM)
import System.Directory
import System.FilePath         ((<.>), (</>))

type ViewDoc = "view"
               :> Capture "docId" DocId
               :> RawM

-- | Make index file path from doc id.
mkPath
  :: DocId
  -> FilePath
mkPath docId = "view" </> docId </> "index" <.> "html"

serveFile
  :: FilePath -- ^ the doc id
  -> Application
serveFile path _ resp = do
  -- document exists, return preview page
  let header = [("Content-Type", "text/html")]
  resp $ responseFile status200 header path Nothing

-- | Send rendered html
viewDoc :: DocId -> ServerT RawM KHandler
viewDoc docId = do
  let fname = mkPath docId

  -- if html doesn't exist, pull the latest version from database (this
  -- prevents a race condition, see Karasu.Pandoc.regenPreview)
  unlessM (liftIO $ doesFileExist fname) $ do
    res <- runDb $ getBy $ UniqueDocId docId
    case res of
      Nothing ->
        throwError err404 { errBody = "Nothing here." }
      Just (Entity _ doc) ->
        liftIO $ writeFileHandleMissing' fname $ docInfoRenderedHtml doc

  return $ serveFile fname
