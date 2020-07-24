{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | API for recompiling all documents
module Karasu.Handlers.ApiRecompileAll (
  RecompileAllApi,
  recompileAll
) where

import Karasu.Database
import Karasu.Environment
import Karasu.Handler
import Karasu.Models
import Karasu.Pandoc
import Karasu.Pandoc.Renderer (HTMLTemplate)

import Control.Monad           (when, void)
import Control.Monad.Reader    (asks)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text               (Text)
import Database.Persist.Sqlite
import GHC.Generics
import Servant

newtype RecompileBody
   = RecompileBody
   { masterPass :: MasterPassword
   }
  deriving (Generic, Show)

instance ToJSON RecompileBody
instance FromJSON RecompileBody

type RecompileAllApi = "api"
                 :> "recompile-all"
                 :> ReqBody '[JSON] RecompileBody
                 :> PostAccepted '[PlainText] Text

recompileDoc
  :: HTMLTemplate
  -> Entity DocInfo
  -> KHandler ()
recompileDoc tmpl (Entity docKey docInfo) = do
  let docId = docInfoDocId docInfo
  liftIO $ putStrLn $ "Recompiling " <> docId
  html <- regenPreview (docInfoDocId docInfo) tmpl (docInfoText docInfo)
  -- update existing htmls in database
  void $ runDb $ updateGet docKey [ DocInfoRenderedHtml =. html ]

-- | Recomepile all documents
recompileAll :: RecompileBody -> KHandler Text
recompileAll docBody = do
  correctPass <- asks envMaster
  -- master password incorrect
  when (correctPass /= masterPass docBody) $
    throwError err403 { errBody = "Nope, try again." }

  -- select all documents
  res <- runDb $ selectList [] []

  -- recompile all the documents
  tmpl <- asks envTemplate
  mapM_ (recompileDoc tmpl) res

  return "Recompilation done!"
