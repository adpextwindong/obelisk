{-# LANGUAGE FlexibleContexts #-}
module Obelisk.Manager.Input where

import qualified SDL
import Control.Monad.State

import Obelisk.Engine.Input
import Obelisk.Wrapper.SDLInput
import Obelisk.State

class Monad m => HasInput m where
    updateInput :: m ()
    setInput :: Input -> m ()
    getInput :: m Input
    updateCamEvents :: [SDL.EventPayload] -> m ()

updateInput' :: (MonadState Vars m, HasInput m, SDLInput m) => m ()
updateInput' = do
    -- input <- getInput
    events <- pollEventPayloads
    setInput (stepControl events initInput) --TODO probably should be init vars so we dont spin endlessly
    updateCamEvents events

getInput' :: MonadState Vars m => m Input
getInput' = gets vInput

setInput' :: MonadState Vars m => Input -> m ()
setInput' input = modify (\v -> v { vInput = input })

--TODO re-express this as a fold
updateCamEvents' :: MonadState Vars m => [SDL.EventPayload] -> m ()
updateCamEvents' ((SDL.MouseMotionEvent (SDL.MouseMotionEventData _window _device buttons position relmotion)) : xs) = modify (\v ->
    if SDL.ButtonLeft `elem` buttons
    then v { camPan = camPan v + fmap fromIntegral relmotion }
    else v) >> updateCamEvents' xs

--TODO should be centered on the mouse but this will require more plumbing
updateCamEvents' ((SDL.MouseWheelEvent (SDL.MouseWheelEventData _window _device (SDL.V2 scrollx scrolly) direction)) : xs) = modify (\v ->
    let mag = fromIntegral scrolly
        pre = camZoom v
        scaleSpeedConstant = 0.05 --TODO tune this
        in
    case direction of
        SDL.ScrollNormal  -> v { camZoom = pre + (scaleSpeedConstant * mag)}
        SDL.ScrollFlipped -> v { camZoom = pre - (scaleSpeedConstant * mag)}) >> updateCamEvents' xs

updateCamEvents' (x:xs) = updateCamEvents' xs
updateCamEvents' [] = return ()

stepControl :: [SDL.EventPayload] -> Input -> Input
stepControl (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym{SDL.keysymKeycode = SDL.KeycodeA }}:xs) (Input _ y z a) = stepControl xs (Input True y z a) --TODO REFACTOR
stepControl (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym{SDL.keysymKeycode = SDL.KeycodeD }}:xs) (Input x _ z a) = stepControl xs (Input x True z a)
stepControl (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym{SDL.keysymKeycode = SDL.KeycodeEscape }}:xs) (Input x y _ a) = stepControl xs (Input x y True a)
stepControl (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym{SDL.keysymKeycode = SDL.KeycodeP }}:xs) (Input x y a _) = stepControl xs (Input x y a True)
stepControl _ x = x
