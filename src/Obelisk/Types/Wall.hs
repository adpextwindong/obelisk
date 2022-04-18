{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, TemplateHaskell #-}
module Obelisk.Types.Wall where

--https://stackoverflow.com/a/38636251

import qualified Data.Vector.Unboxed as U
import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable
import Data.Vector.Unboxed.Deriving
import Data.Word
import qualified SDL

--TODO rename
--TODO add mechanism for SDL.Video.Renderer.copy use in memory textures
data WallType = EW | FW Int SDL.BlendMode | DW --Empty Wall, Full Wall, Door Wall
    deriving (Show, Eq)
