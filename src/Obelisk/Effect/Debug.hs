module Obelisk.Effect.Debug where

import Control.Monad.IO.Class

import Obelisk.Engine.Input
import Obelisk.State

class Monad m => Debug m where
    printGS :: Vars -> m ()

printGS' :: MonadIO m => Vars -> m ()
printGS' = liftIO . print    