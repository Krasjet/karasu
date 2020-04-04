{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | API for creating a document, for private use only
module Karasu.Handlers.ApiEditDoc (EditDocApi, editDoc) where

import Karasu.Database
import Karasu.Handler
import Karasu.Models

import qualified Data.Text.IO as TIO

import Control.Monad.IO.Class (liftIO)
import Servant
import Servant.HTML.Blaze
import System.FilePath        ((<.>), (</>))
import Text.Blaze.Html

type EditDocApi = "edit"
               :> Capture "docId" DocId
               :> Get '[HTML] Html

-- | Send the editor page
editDoc :: DocId -> KHandler Html
editDoc docId = do
  -- raise error if 404
  docExists404 docId

  -- TODO might want to use responseFile to avoid extra buffering
  -- https://github.com/haskell-servant/servant/issues/1281
  html <- liftIO $ TIO.readFile $ "static" </> "editor" <.> "html"
  return $ preEscapedToMarkup html
