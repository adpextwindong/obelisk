module Obelisk.Engine.Input where

data Input = Input
    {
        iLeft :: Bool,
        iRight :: Bool,
        iQuit :: Bool,
        iPrintState :: Bool
    } deriving (Show, Eq)

initInput :: Input
initInput = Input False False False False