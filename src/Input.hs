module Input (
    Input(..)
  , Cinput(..)
  , Minput(..)
  , getInput
  , move
) where

import Prelude hiding(Either(Left,Right))

import Util
import Unit

import qualified Data.Map.Strict as Map
import Data.Maybe(fromJust)
import Data.List(intersperse)

data Input = Cmd Cinput | Move Minput
  deriving (Show, Eq)

data Cinput =
    Quit
  deriving (Show, Eq)

data Minput =
    Wait
  | Up
  | Down
  | Left
  | Right
  | UpL
  | UpR
  | DownL
  | DownR
  deriving (Show, Eq)

getInput :: IO Input
getInput = do
    c <- getChar
    case keymap c of
        Just i  -> return i
        Nothing -> getInput

keymap :: Char -> Maybe Input
keymap c = Map.lookup c keys

keys = Map.fromList [
    ('Q', Cmd Quit)
    -- movement
  , ('s', Move Wait)
  , ('j', Move Down)     , ('k', Move Up)
  , ('h', Move Left)     , ('l', Move Right)
  , ('y', Move UpL)      , ('u', Move UpR)
  , ('b', Move DownL)    , ('n', Move DownR)
  ]

move :: XY -> Minput -> XY
move (x,y) m = case m of
                 Wait  -> (x,y)
                 Up    -> (x,y-1)
                 Down  -> (x,y+1)
                 Left  -> (x-1,y)
                 Right -> (x+1,y)
                 UpL   -> (x-1,y-1)
                 UpR   -> (x+1,y-1)
                 DownL -> (x-1,y+1)
                 DownR -> (x+1,y+1)

