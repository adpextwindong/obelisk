{-# LANGUAGE FlexibleContexts #-}
module Obelisk.Runner where

import Control.Monad.Reader
import Control.Monad.State
import Linear
import Control.Lens
import qualified SDL

import Obelisk.Config
import Obelisk.State
import Obelisk.Effect.Renderer
import Obelisk.Wrapper.SDLInput
import Obelisk.Engine.Input
import Obelisk.Manager.Input
import Obelisk.Math.Homogenous


mainLoop :: ( MonadReader Config m
            , MonadState Vars m
            , SDLInput m
            , HasInput m
            , Renderer m ) => m ()
mainLoop = do
    updateInput
    clearScreen
    fillBackground

    -- quitSignal <- checkQuitSignal
    --TODO process input update to rotation
    input <- getInput
    old_gs <- get
    let dir = direction $ player old_gs :: V2 Double
    let cplane = camera_plane $ player old_gs

    let rotationT = rotation2 0.05
    let quitSignal = iQuit input
    let (rotated_dir, rotated_cplane) = if iLeft input
                   then (dir *! rotationT, cplane *! rotationT)
                   else (dir, cplane)
    modify $ pVars %~ (\v -> v { direction = rotated_dir, camera_plane = rotated_cplane})

    time <- getTime
    let elapsed_seconds = fromIntegral (toInteger time) / 1000.0
    let rotationFactor = elapsed_seconds --0.0
    
    --gameTick hs TODO updateStep
    

    --TODO once we finish debug stuff and get drawing done
        --Implement press tab to show debug screen

    gs <- get
    drawDebug gs
    drawScreen 
    
    fillBackground
    
    unless quitSignal mainLoop