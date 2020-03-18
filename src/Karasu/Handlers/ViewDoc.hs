{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

-- | The route for compiled documents
module Karasu.Handlers.ViewDoc (ViewDoc) where

import Servant

type ViewDoc = "view" :> Raw
