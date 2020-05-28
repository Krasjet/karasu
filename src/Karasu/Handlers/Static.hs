-- | The route for serving static files, only for testing. On deployment
-- server, this is handled by nginx.
module Karasu.Handlers.Static (StaticFiles) where

import Servant

type StaticFiles = Raw
