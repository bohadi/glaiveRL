module World ( 
    World(..)
  , currentLevel
) where

import Util
import Unit
import Object
import Player
import Level

import Data.Maybe(fromJust)

data World = World {
    flags  :: [(String, Bool)]
  , levels :: [(String, Level)]
}

currentLevel :: World -> Level
currentLevel w = fromJust $ lookup "cur" $ levels w
