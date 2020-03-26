{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

-- | The route for compiled documents
-- TODO remove this since nginx can handle static files now
module Karasu.Handlers.ViewDoc (ViewDoc) where

import Servant

type ViewDoc = "view" :> Raw
