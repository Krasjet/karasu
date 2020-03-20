{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}

-- | API for markdown -> HTML preview
module Karasu.Handlers.ApiPreviewDoc (PreviewDocApi, previewDoc) where

import Karasu.Database
import Karasu.Handler
import Karasu.Models
import Karasu.Pandoc.Renderer

import qualified Data.ByteString.Lazy.Char8 as LB8

import Control.Monad.IO.Class  (liftIO)
import Data.Aeson
import GHC.Generics
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html

data PreviewDocBody = PreviewDocBody {
  docId      :: DocId,
  markdown   :: Markdown,
  accessCode :: Maybe AccessCode
} deriving (Generic, Show)

instance ToJSON   PreviewDocBody
instance FromJSON PreviewDocBody

type PreviewDocApi = "api"
                  :> "preview"
                  :> ReqBody '[JSON] PreviewDocBody
                  :> Post '[HTML] Html -- apparently GET request cannot have a body

-- | Process markdown and send preview
previewDoc :: PreviewDocBody ->  KHandler Html
previewDoc prevBody = do
  let dId = docId prevBody
  -- first validate the access code
  validateDoc dId (accessCode prevBody)

  -- render the markdown file
  let md = markdown prevBody
  out <- liftIO $ renderPreview dId md
  case out of
    Left err   -> throwError err400 { errBody = LB8.pack $ show err }
    Right html -> return html
