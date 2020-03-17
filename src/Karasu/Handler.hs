-- | Our own custom handler
module Karasu.Handler (KHandler, nt) where

import Karasu.Environment (KarasuEnv)

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Servant                    (Handler)

-- | A wrapper for servant Handler
-- This allows us to compose monads on top of Handler (might expand later)
type KHandler = ReaderT KarasuEnv Handler

-- | The natural transformation KHandler ~> Handler
nt
  :: KarasuEnv  -- ^ the runtime environment
  -> KHandler a
  -> Handler a
nt env kHdl = runReaderT kHdl env
