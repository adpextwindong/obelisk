module Obelisk.Effect.Debug where

import Control.Monad.IO.Class

import Obelisk.Engine.Input
import Obelisk.State

class Monad m => Debug m where
    printGS :: Vars -> m ()
    dprint :: (Show a) => a -> m ()

--TODO we should do a custom pretty printing so we don't spew the gamemap all the time
printGS' :: MonadIO m => Vars -> m ()
printGS' = liftIO . print    

print' :: (Show a , MonadIO m) => a -> m ()
print' = liftIO . print