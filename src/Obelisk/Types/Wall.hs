{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, TemplateHaskell #-}
module Obelisk.Types.Wall where

--https://stackoverflow.com/a/38636251

import qualified Data.Vector.Unboxed as U
import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable
import Data.Vector.Unboxed.Deriving
import Data.Word

--TODO rename
data WallType = EW | FW | DW --Empty Wall, Full Wall, Door Wall
    deriving (Show, Eq)

wallToWord8 :: WallType -> Word8
wallToWord8 w =
    case w of
        EW -> 0
        FW -> 1
        DW -> 2

word8ToWall :: Word8 -> WallType
word8ToWall w =
    case w of
        0 -> EW
        1 -> FW
        _ -> DW

derivingUnbox "WallType"
    [t| WallType -> Word8 |]
    [| wallToWord8 |]
    [| word8ToWall |]