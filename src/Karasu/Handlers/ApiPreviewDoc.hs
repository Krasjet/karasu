{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | API for markdown -> HTML preview
module Karasu.Handlers.ApiPreviewDoc (PreviewDocApi, previewDoc) where

import Karasu.Database
import Karasu.Handler
import Karasu.Models

import qualified Data.ByteString.Lazy.Char8 as LB8

import Control.Monad           (when)
import Control.Monad.IO.Class  (liftIO)
import Data.Aeson
import Database.Persist.Sqlite
import GHC.Generics
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html
import Text.Pandoc

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
  liftIO $ print prevBody
  let dId = docId prevBody
  -- check access code
  res <- runDb $ getBy $ UniqueDocId dId
  case res of
    Nothing -> throwError err403 { errBody = "Something wrong with the docId." }
    Just (Entity _ doc) ->
      -- yes, no accessCode doesn't mean no protection
      when (docInfoAccCode doc /= accessCode prevBody) $
        throwError err403 { errBody = "Nope, try again." }
  -- now, start rendering the markdown file (TODO lift to a Pandoc package)
  let md = markdown prevBody

  let out = runPure $ do
              pandoc <- readMarkdown def md
              writeHtml5 def pandoc
  case out of
    Left err   -> throwError err400 { errBody = LB8.pack $ show err }
    Right html -> return $ preEscapedToMarkup html
