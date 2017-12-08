module Draw ( 
    render
) where

import Util
import Unit
import Player
import Level
import World

import Data.Maybe(fromJust)
import Data.List(intersperse)

render :: Player -> World -> IO ()
render p w = do 
    let l = layout $ fromJust $ lookup "cur" $ levels w
    let l' = placeUnits p l
    mapM_ putStrLn $ doubleWidth <$> l'

doubleWidth :: [Char] -> [Char]
doubleWidth [] = []
doubleWidth (x:xs)
    | inWall x  = x : (sym "H") : doubleWidth xs
    | otherwise = x : (sym "dirt") : doubleWidth xs

inWall :: Char -> Bool
inWall x = elem x $ sym "H" : sym "DR" : sym "UR" : []

placeUnits :: Player -> Layout -> Layout
placeUnits p l = l' where
    l' = l
