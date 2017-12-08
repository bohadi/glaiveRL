module Level ( 
    Level(..)
  , Layout
  , createRoom
) where

import Util
import Unit
import Player

import qualified Data.Map.Strict as Map
import Data.List(transpose)

data Level = Level {
    dim    :: XY
  , layout :: Layout
}
type Layout = [[Char]]

enclose :: [[Char]] -> [[Char]]
enclose l = transpose l'' where
    dim = length $ l !! 0
    top = replicate dim (sym "H")
    bot = replicate dim (sym "H")
    l' = (init $ top : (tail l)) ++ [bot]
    left  = (sym "DR") : (replicate (dim-2) (sym "V")) ++ (sym "UR"):[]
    right = (sym "DL") : (replicate (dim-2) (sym "V")) ++ (sym "UL"):[]
    l'' = (init $ left : (tail $ transpose l')) ++ [right]

createRoom :: Int -> Level
createRoom seed = Level dim layout where
    dim = (seed, seed)
    layout = enclose $ replicate (snd dim) $ replicate (fst dim) $ sym "serpent"
             
