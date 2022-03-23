{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

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
import Obelisk.Graphics.UIScene
import Obelisk.Graphics.DebugUI

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
import Obelisk.Manager.Input (updateCamEvents')

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

mouseTestMain = grenderMouseLook mouseLookRaycastGraphicM

testPresentation = [
    UIScene {
        scene_name = "Title Page",
        graphic_elems = [Prim (FillCircle (V2 1 1) 30 (V4 255 191 0 255))]
    },
    UIScene {
        scene_name = "Second Page",
        graphic_elems = [Prim (FillCircle (V2 2 2) 45 (V4 242 210 189 255))]
    }]

zipperMain :: Presentation -> IO ()
zipperMain presentation = do
    --TODO scene zipper with test slides and some controls for moving between slides
    SDL.initialize [SDL.InitVideo]
    SDL.Font.initialize
    let title = "Graphic Zipper Test"

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

    runObelisk cfg initVars (presentationRenderLoop presentation)

    SDL.Font.free font
    SDL.freeSurface screenSurface
    SDL.destroyWindow window
    SDL.quit
    return ()

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

grender :: Graphic Float -> IO ()
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

grenderMouseLook :: (m ~ Obelisk) => (V2 Float -> m (Graphic Float)) -> IO ()
grenderMouseLook g = do
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

    runObelisk cfg initVars (gRenderMouseLookLoop g)

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
    fillRectangle = fillRectangle'
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
    updateCamEvents = updateCamEvents'

instance Renderer Obelisk where
    clearScreen = clearScreen'
    drawScreen = drawScreen'
    fillBackground = fillBackground'
    drawDebug = drawDebug'
    drawGraphicDebug = drawGraphicDebug'
    drawGraphicDebugWithMatrix = drawGraphicDebugWithMatrix'
    blitSurfaceToWindowSurface = blitSurfaceToWindowSurface'

instance Debug Obelisk where
    printGS = printGS'
    dprint = print'
    getUTCTime = getUTCTime'

instance SDLFont Obelisk where
    renderSolidText = renderSolidText'
