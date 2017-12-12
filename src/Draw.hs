module Draw ( 
    render
) where

import Util
import Unit
import Player
import Level
import World

import Data.Maybe(fromJust)
import Data.List(intersperse, foldl')

render :: Player -> World -> IO ()
render p w = do 
    let lvl = fromJust $ lookup "cur" $ levels w
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
placeCharAt l (c,(x,y)) = l' where
    l' = l --TODO

placeUnits :: Player -> [Unit] -> Layout -> Layout
placeUnits p us l = placeCharAt l' ((sym "pc"), (pos p)) where
    l' = foldl' placeCharAt l $ (\(n,_,xy) -> (sym n, xy)) <$> us
