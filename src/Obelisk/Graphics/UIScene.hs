module Obelisk.Graphics.UIScene where

import Obelisk.Graphics.Primitives

type Presentation = [UIScene]

data UIScene = UIScene {
                scene_name :: String
                ,graphic_elems :: [Graphic (Shape Float)]
                --,text_elems :: [Graphic Text] --TODO text graphics
            } deriving Show