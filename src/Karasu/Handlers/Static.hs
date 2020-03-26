-- | The route for serving static files
-- TODO remove this since nginx can handle static files now
module Karasu.Handlers.Static (StaticFiles) where

import Servant

type StaticFiles = Raw
