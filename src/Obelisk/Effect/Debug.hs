module Obelisk.Effect.Debug where

import Control.Monad.IO.Class
import Data.Time.Clock.System
import Data.Time.Clock

import Obelisk.Engine.Input
import Obelisk.State

class Monad m => Debug m where
    printGS :: Vars -> m ()
    dprint :: (Show a) => a -> m ()
    getUTCTime :: m UTCTime

--TODO we should do a custom pretty printing so we don't spew the gamemap all the time
printGS' :: MonadIO m => Vars -> m ()
printGS' = liftIO . print

print' :: (Show a , MonadIO m) => a -> m ()
print' = liftIO . print

getUTCTime' :: MonadIO m => m UTCTime
getUTCTime' = liftIO $ systemToUTCTime <$> getSystemTime
