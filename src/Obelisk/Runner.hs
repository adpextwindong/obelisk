{-# LANGUAGE FlexibleContexts #-}
module Obelisk.Runner where

import Control.Monad.Reader
import Control.Monad.State

import Obelisk.Config
import Obelisk.State
import Obelisk.Effect.Renderer
import Obelisk.Wrapper.SDLInput

import qualified SDL

mainLoop :: ( MonadReader Config m
            , MonadState Vars m
            , SDLInput m
            , Renderer m ) => m ()
mainLoop = do
    clearScreen
    fillBackground

    quitSignal <- checkQuitSignal
    --SDL.surfaceBlit garg Nothing screenSurface Nothing
    time <- getTime

    let elapsed_seconds = fromIntegral (toInteger time) / 1000.0
    let rotationFactor = elapsed_seconds --0.0
    
    --gameTick hs TODO updateStep
    
    gs <- get

    --TODO once we finish debug stuff and get drawing done
        --Implement press tab to show debug screen
    drawDebug gs
    drawScreen 
    
    fillBackground
    
    unless quitSignal mainLoop