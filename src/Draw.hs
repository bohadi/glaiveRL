module Draw ( 
    render
) where

import Util
import Unit
import Player
import Level
import World

import Data.List(intersperse, foldl')
import qualified Data.Map.Strict as Map

render :: Player -> World -> IO ()
render p w = do 
    let lvl = currentLevel w
    let lay = placeUnits p (units lvl) (layout lvl)
    mapM_ putStrLn $ doubleWidth <$> asStringList lay

doubleWidth :: [Char] -> [Char]
doubleWidth [] = []
doubleWidth (x:xs)
    | inWall x  = x : (sym "H") : doubleWidth xs
    | otherwise = x : (sym "dirt") : doubleWidth xs

inWall :: Char -> Bool
inWall x = elem x $ sym "H" : sym "DR" : sym "UR" : []

placeCharAt :: Layout -> (Char, XY) -> Layout
placeCharAt l (c,(x,y)) = Map.insert (x,y) c l

placeUnits :: Player -> [Unit] -> Layout -> Layout
placeUnits p us l = foldl' placeCharAt l nXYs where
    nXYs = ((sym "pc"), (pos p)) : ( (\(n,_,xy)->(sym n, xy)) <$> us )
