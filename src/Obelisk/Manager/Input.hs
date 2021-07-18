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

updateInput' :: (HasInput m, SDLInput m) => m ()
updateInput' = do
    -- input <- getInput
    events <- pollEventPayloads
    setInput (stepControl events initInput) --TODO probably should be init vars so we dont spin endlessly

getInput' :: MonadState Vars m => m Input
getInput' = gets vInput

setInput' :: MonadState Vars m => Input -> m ()
setInput' input = modify (\v -> v { vInput = input })

stepControl :: [SDL.EventPayload] -> Input -> Input
stepControl (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym{SDL.keysymKeycode = SDL.KeycodeA }}:xs) (Input _ y z a) = stepControl xs (Input True y z a) --TODO REFACTOR
stepControl (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym{SDL.keysymKeycode = SDL.KeycodeD }}:xs) (Input x _ z a) = stepControl xs (Input x True z a)
stepControl (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym{SDL.keysymKeycode = SDL.KeycodeEscape }}:xs) (Input x y _ a) = stepControl xs (Input x y True a)
stepControl (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym{SDL.keysymKeycode = SDL.KeycodeP }}:xs) (Input x y a _) = stepControl xs (Input x y a True)
stepControl _ x = x
