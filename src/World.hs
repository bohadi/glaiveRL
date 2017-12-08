module World ( 
    World(..)
) where

import Util
import Unit
import Player
import Level

data World = World {
    flags  :: [(String, Bool)]
  , levels :: [(String, Level)]
}
