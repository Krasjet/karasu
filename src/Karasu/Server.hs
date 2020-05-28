{-# LANGUAGE OverloadedStrings #-}

-- | All the server related stuff
module Karasu.Server (karasuServer, staticServer, apiServer) where

import Karasu.Api
import Karasu.Environment
import Karasu.Handler
import Karasu.Handlers.ApiCreateDoc
import Karasu.Handlers.ApiEditDoc
import Karasu.Handlers.ApiGetDoc
import Karasu.Handlers.ApiPreviewDoc
import Karasu.Handlers.ApiSaveDoc
import Karasu.Handlers.Static
import Karasu.Handlers.ViewDoc
import Network.Wai.Application.Static
import WaiAppStatic.Types

import Servant

-- | settings for static server
staticSettings :: FilePath -> StaticSettings
staticSettings root = (defaultWebAppSettings root)
  { ssIndices = map unsafeToPiece ["index.html"]
  , ssRedirectToIndex = True
  , ssAddTrailingSlash = True
  }

-- * Static server
-- | which serves all the static files
staticServer :: Server StaticFiles
staticServer = serveDirectoryWith $ staticSettings "static/"

-- * API server
-- which serves all APIs, of course

-- | API server with KHandler wrapper
--   We will need to use hoistServer to apply the
--   natural transformation KHandler ~> Handler
apiServerK :: ServerT ReqApi KHandler
apiServerK = createDoc :<|> editDoc :<|> getDoc :<|> previewDoc :<|> saveDoc :<|> viewDoc

-- | Transformed server
apiServer :: KarasuEnv -> Server ReqApi
apiServer env = hoistServer reqApi (nt env) apiServerK

-- * Combined server
karasuServer :: KarasuEnv -> Server KarasuApi
karasuServer env = apiServer env :<|> staticServer
