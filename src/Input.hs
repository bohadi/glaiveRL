module Input ( 
    Input(..)
  , getInput
) where

import Prelude hiding(Either(Left,Right))

import Util
import Unit
import Player
import Level
import World

import qualified Data.Map.Strict as Map
import Data.Maybe(fromJust)
import Data.List(intersperse)

data Input =
    Quit
  | Wait
  | Up
  | Down
  | Left
  | Right
  | UpL
  | UpR
  | DownL
  | DownR
  deriving (Show, Eq, Ord)

getInput :: IO Input
getInput = do
    c <- getChar
    case keymap c of
        Just i  -> return i
        Nothing -> getInput

keymap :: Char -> Maybe Input
keymap c = Map.lookup c keys

keys = Map.fromList $ [
    ('Q', Quit)     , ('s', Wait)
    -- movement
  , ('j', Up)       , ('k', Down)
  , ('h', Left)     , ('l', Right)
  , ('t', UpL)      , ('y', UpR)
  , ('b', DownL)    , ('n', DownR)
  ]
