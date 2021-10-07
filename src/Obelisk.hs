{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Obelisk where

import Obelisk.Math.Homogenous

import Obelisk.Runner
import Obelisk.Config
import Obelisk.State
import Obelisk.Effect.Renderer
import Obelisk.Effect.Debug
import Obelisk.Manager.Input
import Obelisk.Wrapper.SDLRenderer
import Obelisk.Wrapper.SDLInput
import Obelisk.Wrapper.SDLFont
import Obelisk.Graphics.Primitives

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Catch

import Control.Concurrent (threadDelay)
import Foreign.C.Types ( CInt )
import qualified Data.Text as T
import qualified SDL.Vect as SDL
import qualified SDL
import qualified SDL.Event as SDL
import qualified SDL.Video.Renderer as SDL

import qualified SDL.Font


import Data.Word
import Linear
import Control.Lens
import SDL.Primitive as SDL

--ACCESSED godBoltMap !! y !! x style
{-
gameTick :: ScreenHandles -> StateT Vars IO ()
gameTick hs = do
    --TODO gsUpdate :: StateT Vars IO ()
    newgs <- get
    lift $ drawDebug hs newgs
    return ()
-}

--TODO make minimap rotate around the player
--TODO mouse zoom handling

(initialScreenWidth, initialScreenHeight) = (640, 480) :: (CInt,CInt)
main :: IO ()
main = do
    SDL.initialize [SDL.InitVideo]
    SDL.Font.initialize
    let title = "World Debug Window"

    --TODO fetch system fonts later so we dont have to redistribute ttfs
    font <- SDL.Font.load "resources/arial.ttf" 16

    window <- SDL.createWindow title SDL.defaultWindow { SDL.windowInitialSize = V2 initialScreenWidth initialScreenHeight }
    SDL.showWindow window

    screenSurface <- SDL.getWindowSurface window
    SDL.updateWindowSurface window

    screenRenderer <- SDL.createSoftwareRenderer screenSurface :: IO SDL.Renderer

    let hs = (window, screenSurface, screenRenderer)

    let cfg = Config {
                cWindow = window,
                cRenderer = screenRenderer,
                cSurface = screenSurface,
                cScreenWidth = initialScreenWidth,
                cScreenHeight = initialScreenHeight,
                cFont = font
            }

    runObelisk cfg initVars mainLoop

    SDL.Font.free font
    SDL.freeSurface screenSurface
    SDL.destroyWindow window
    SDL.quit

grender :: Graphic (Shape Float) -> IO ()
grender g = do
    SDL.initialize [SDL.InitVideo]
    SDL.Font.initialize
    let title = "Graphic Renderer Test"

    font <- SDL.Font.load "resources/arial.ttf" 16
    window <- SDL.createWindow title SDL.defaultWindow { SDL.windowInitialSize = V2 initialScreenWidth initialScreenHeight }
    SDL.showWindow window
    screenSurface <- SDL.getWindowSurface window
    SDL.updateWindowSurface window

    screenRenderer <- SDL.createSoftwareRenderer screenSurface :: IO SDL.Renderer

    let hs = (window, screenSurface, screenRenderer)

    let cfg = Config {
                cWindow = window,
                cRenderer = screenRenderer,
                cSurface = screenSurface,
                cScreenWidth = initialScreenWidth,
                cScreenHeight = initialScreenHeight,
                cFont = font
            }

    runObelisk cfg initVars (grenderLoop g)

    SDL.Font.free font
    SDL.freeSurface screenSurface
    SDL.destroyWindow window
    SDL.quit


newtype Obelisk a = Obelisk (ReaderT Config (StateT Vars IO) a)
    deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO, MonadThrow, MonadCatch)

runObelisk :: Config -> Vars -> Obelisk a -> IO a
runObelisk config v (Obelisk m) = evalStateT (runReaderT m config) v

instance SDLRenderer Obelisk where
    updateWindowSurface = updateWindowSurface'
    surfaceFillScreenRect = surfaceFillScreenRect'
    clearRenderer = clearRenderer'
    drawLine = drawLine'
    fillTriangle = fillTriangle'
    circle = circle'
    fillCircle = fillCircle'
    surfaceBlit = surfaceBlit'

instance SDLInput Obelisk where
    pollEventPayloads = pollEventPayloads'
    checkQuitSignal = checkQuitSignal'
    getTime = getTime'
    getMouseAbsoluteLoc = getMouseAbsoluteLoc'

instance HasInput Obelisk where
    updateInput = updateInput'
    getInput = getInput'
    setInput = setInput'

instance Renderer Obelisk where
    clearScreen = clearScreen'
    drawScreen = drawScreen'
    fillBackground = fillBackground'
    drawDebug = drawDebug'
    drawGraphicDebug = drawGraphicDebug'

instance Debug Obelisk where
    printGS = printGS'
    dprint = print'

instance SDLFont Obelisk where
    renderSolidText = renderSolidText'