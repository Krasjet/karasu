{-# LANGUAGE OverloadedStrings #-}

-- | The route for serving static files, only for testing. On deployment
-- server, this is handled by nginx.
module Karasu.Handlers.Static (
  StaticFiles,
  handle404,
) where

import Servant
import Network.Wai
import Network.HTTP.Types

type StaticFiles = Raw

handle404 :: Application
handle404 _ resp = resp $
  responseLBS status404 [("Content-Type", "text/plain")] "Nothing here."
