{-# LANGUAGE FlexibleContexts #-}
module Obelisk.Manager.Input where

import qualified SDL
import qualified SDL.Raw.Types (Point)
import qualified SDL.Raw.Event as SDL.Raw
import Control.Monad.State
import Control.Monad.Reader
import Linear
import Control.Lens
import Data.Array ((//))
import GHC.Int

import Obelisk.Math.Homogenous
import Obelisk.Engine.Input
import Obelisk.Wrapper.SDLInput
import Obelisk.Effect.Renderer
import Obelisk.Effect.Debug
import Obelisk.State
import Obelisk.Types.Wall
import Obelisk.Config

class Monad m => HasInput m where
    updateInput :: m ()
    setInput :: Input -> m ()
    getInput :: m Input
    updateCamEvents :: [SDL.EventPayload] -> m ()

updateInput' :: (SDLCanDraw m, MonadIO m,MonadState Vars m, HasInput m, SDLInput m) => m ()
updateInput' = do
    -- input <- getInput
    events <- pollEventPayloads
    setInput (stepControl events initInput) --TODO probably should be init vars so we dont spin endlessly
    updateCamEvents events
    checkCamSwap events

getInput' :: MonadState Vars m => m Input
getInput' = gets vInput

setInput' :: MonadState Vars m => Input -> m ()
setInput' input = modify (\v -> v { vInput = input })

checkCamSwap :: (MonadState Vars m, MonadIO m, HasInput m) => [SDL.EventPayload] -> m ()
checkCamSwap (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym{SDL.keysymKeycode = SDL.KeycodeF1 }}:xs) = do
  s <- get
  case viewMode s of
    OverheadDebug -> SDL.Raw.setRelativeMouseMode True
    PlayerPOV     -> SDL.Raw.setRelativeMouseMode False

  modify (\s -> s { viewMode = case viewMode s of
                                  OverheadDebug -> PlayerPOV
                                  PlayerPOV -> OverheadDebug
  })
checkCamSwap (_:xs) = checkCamSwap xs
checkCamSwap [] = return ()

rotatePlayer :: Vars -> GHC.Int.Int32 -> Vars
rotatePlayer v rv =
  let mouseMove = fromIntegral $ rv
      sens = 0.001 --TUNE parameter
      mouseTurn = rotation2 $ -sens * mouseMove
      rotatePlayer player = player {
                              direction = direction player *! mouseTurn,
                              camera_plane = camera_plane player *! mouseTurn
                            } in

      if(mouseMove == 0) then v else v { player = rotatePlayer $ player v }


--TODO middle click to change transparency
updateCamEvent :: (SDLCanDraw m,Debug m, SDLInput m, MonadState Vars m) => SDL.EventPayload -> m ()
updateCamEvent (SDL.MouseButtonEvent e@(SDL.MouseButtonEventData _window _motion _which button clicks aLoc)) = do
    textureCount <- asks cTextureCount
    modify (\v ->
      if (SDL.ButtonRight == button || SDL.ButtonMiddle == button)  && SDL.mouseButtonEventMotion e == SDL.Released
      then
        let gtp = worldGTP v
            --TODO bounds check this
            worldLoc = floor <$> rawPDtoWorldPos gtp (fmap fromIntegral aLoc) in

            case button of
                SDL.ButtonRight -> v { world = incrementWall textureCount worldLoc (world v) }
                SDL.ButtonMiddle -> v { world = incrementTransparency worldLoc (world v) }
                _ -> v
      else v)

updateCamEvent (SDL.MouseMotionEvent (SDL.MouseMotionEventData _window _device buttons position (V2 relmotionx relmotiony))) = modify (\v ->
    --TODO rotate player
    case viewMode v of
      OverheadDebug -> if SDL.ButtonLeft `elem` buttons
                       then v { worldGTP = translate (fromIntegral relmotionx) (fromIntegral relmotiony) !*! worldGTP v }
                       else v
      PlayerPOV -> rotatePlayer v relmotionx)


updateCamEvent (SDL.MouseWheelEvent (SDL.MouseWheelEventData _window _device (SDL.V2 scrollx scrolly) direction)) = do
    vars <- get
    let gtp = worldGTP vars

    aLoc@(SDL.P absMouseLoc) <- fmap fromIntegral <$> getMouseAbsoluteLoc
    let worldLoc = rawPDtoWorldPos gtp aLoc

    modify (\v ->
      let mag = fromIntegral scrolly
          scaleSpeedConstant = 0.15 --TODO store tunable in config
          in
      case direction of
          --TODO stash scaleConstant
          SDL.ScrollNormal  -> v { worldGTP = zoomAround (1.0 + (scaleSpeedConstant * mag)) absMouseLoc !*! worldGTP v }
          SDL.ScrollFlipped -> v { worldGTP = zoomAround (1.0 + (-(scaleSpeedConstant * mag))) absMouseLoc !*! worldGTP v} )

updateCamEvent _ = return ()
updateCamEvents' xs = mapM_ updateCamEvent xs

stepControl :: [SDL.EventPayload] -> Input -> Input
stepControl (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym{SDL.keysymKeycode = SDL.KeycodeA }}:xs) (Input _ y z a) = stepControl xs (Input True y z a) --TODO REFACTOR
stepControl (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym{SDL.keysymKeycode = SDL.KeycodeD }}:xs) (Input x _ z a) = stepControl xs (Input x True z a)
stepControl (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym{SDL.keysymKeycode = SDL.KeycodeEscape }}:xs) (Input x y _ a) = stepControl xs (Input x y True a)
stepControl (SDL.KeyboardEvent SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym{SDL.keysymKeycode = SDL.KeycodeP }}:xs) (Input x y a _) = stepControl xs (Input x y a True)
stepControl _ x = x

incrementWall :: Int -> V2 Int -> WorldTiles -> WorldTiles
incrementWall textureCount inds@(V2 x y) w@(WorldTiles tiles ws) = (
  case accessMapV w inds of
    EW -> w { mapTiles = tiles // [(accessIndex w x y, FW 0 SDL.BlendNone)] }
    FW i _ -> if i + 1 < textureCount
            then w { mapTiles = tiles // [(accessIndex w x y, FW (i + 1) SDL.BlendNone)] }
            else w { mapTiles = tiles // [(accessIndex w x y, EW)] })

incrementTransparency :: V2 Int -> WorldTiles -> WorldTiles
incrementTransparency ind@(V2 x y) w@(WorldTiles tiles _)  =
    case accessMapV w ind of
        EW -> w
        FW i SDL.BlendNone        -> w { mapTiles = tiles // [(accessIndex w x y, FW i SDL.BlendAlphaBlend)] }
        FW i SDL.BlendAlphaBlend  -> w { mapTiles = tiles // [(accessIndex w x y, FW i SDL.BlendAdditive)] }
        FW i SDL.BlendAdditive    -> w { mapTiles = tiles // [(accessIndex w x y, FW i SDL.BlendMod)] }
        FW i SDL.BlendMod         -> w { mapTiles = tiles // [(accessIndex w x y, FW i SDL.BlendNone)] }
