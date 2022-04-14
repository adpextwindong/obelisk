module Obelisk.Graphics.ColorConstants where

import qualified SDL
import qualified SDL.Primitive as SDL

import Obelisk.Types.Wall

-- UI CONSTANTS
gridColor :: SDL.Color
gridColor = SDL.V4 63 63 63 maxBound

white :: SDL.Color
white = SDL.V4 maxBound maxBound maxBound maxBound

red :: SDL.Color
red = SDL.V4 maxBound 0 0 maxBound

arrowColor :: SDL.Color
arrowColor = SDL.V4 255 51 51 maxBound

blue :: SDL.Color
blue = SDL.V4 0 0 maxBound maxBound

black :: SDL.Color
black = SDL.V4 0 0 0 0

yellow :: SDL.Color
yellow = SDL.V4 255 255 0 maxBound

--GodBolt Colors
backgroundColor :: SDL.Color
backgroundColor = SDL.V4 34 34 34 maxBound

filledTileColor :: SDL.Color
filledTileColor = SDL.V4 51 51 102 maxBound

doorTileColor :: SDL.Color
doorTileColor = SDL.V4 102 51 102 maxBound

wallTypeToColor :: WallType -> SDL.Color
wallTypeToColor (FW _ _) = filledTileColor
wallTypeToColor EW = backgroundColor
wallTypeToColor DW = doorTileColor
