{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | API for creating a document, for private use only
module Karasu.Handlers.ApiEditDoc (EditDocApi, editDoc) where

-- import Karasu.Database
-- import Karasu.Handler
import Karasu.Models
import Karasu.Environment

import Network.Wai
import Data.Tagged
import Network.HTTP.Types
import Servant
import System.FilePath          ((<.>), (</>))

type EditDocApi = "edit"
               :> Capture "docId" DocId
               :> Raw

editDocApp :: KarasuEnv -> DocId -> Application
editDocApp _env _docId _ resp = do
  -- raise error if 404
  -- docExists404 docId

  let header = [("Content-Type", "text/html")]
  resp $ responseFile status200 header ("static" </> "editor" <.> "html") Nothing

-- | Send the editor page
editDoc :: KarasuEnv -> DocId -> Server Raw
editDoc = (Tagged .) . editDocApp
