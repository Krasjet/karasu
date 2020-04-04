{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | A summary of all the API definitions
module Karasu.Api (KarasuApi, ReqApi, karasuApi, reqApi) where

import Karasu.Handlers.ApiCreateDoc
import Karasu.Handlers.ApiEditDoc
import Karasu.Handlers.ApiGetDoc
import Karasu.Handlers.ApiPreviewDoc
import Karasu.Handlers.ApiSaveDoc
import Karasu.Handlers.Static
import Karasu.Handlers.ViewDoc

import Servant

-- | API for requests
type ReqApi = CreateDocApi :<|> EditDocApi :<|> GetDocApi :<|> PreviewDocApi :<|> SaveDocApi
-- | combined APIs for the app
type KarasuApi = ReqApi :<|> ViewDoc :<|> StaticFiles

reqApi :: Proxy ReqApi
reqApi = Proxy

karasuApi :: Proxy KarasuApi
karasuApi = Proxy
