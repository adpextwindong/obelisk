module Obelisk.Effect.Renderer where

import Control.Monad.Reader

import Obelisk.Config

class Monad m => Renderer m where
    drawGrid :: m ()
    drawGridTiles :: m ()
    drawPlayer :: m ()

