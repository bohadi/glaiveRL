module Draw ( 
    render
) where

import Util
import Unit
import Object
import Player
import Level
import World

import Data.List(intersperse, foldl')
import qualified Data.Map.Strict as Map
import Control.Lens

render :: Player -> World -> IO ()
render p w =
     mapM_ putStrLn $ doubleWidth <$> asStringList (placeDyna p w)

doubleWidth :: [Glyph] -> [Glyph]
doubleWidth [] = []
doubleWidth (x:xs)
    | inWall  x  = x : sym "H"     : doubleWidth xs
    | inRock  x  = x : sym "rock"  : doubleWidth xs
    | inGrass x  = x : sym "grass" : doubleWidth xs
    | otherwise  = x : sym "dirt"  : doubleWidth xs

inWall  x = x `elem` [sym "H", sym "DR", sym "UR"]
inRock  x = x == sym "rock"
inGrass x = x == sym "grass"

placeDyna :: Player -> World -> Layout
placeDyna p w = lopu where
    lvl  = currentLevel w
    lo   = placeObjects (Map.toList $ objects lvl) (layout lvl)
    lopu = placePlayerAndUnits p (Map.toList $ units lvl) lo

placeObjects :: [(XY,Object)] -> Layout -> Layout
placeObjects os l = foldl' placeGlyphAt l gXYs where
    gXYs = (\(xy, (_,gid))->(xy, sym gid)) <$> os

placePlayerAndUnits :: Player -> [(XY,Unit)] -> Layout -> Layout
placePlayerAndUnits p us l = foldl' placeGlyphAt l gXYs where
    gXYs = (p ^. pos, sym "pc")
         : ((\(xy,(_,gid,_))->(xy, sym gid)) <$> us)

placeGlyphAt :: Layout -> (XY, Glyph) -> Layout
placeGlyphAt l ((x,y), g) = Map.insert (x,y) g l

