module Level ( 
    Level(..)
  , Dyna
  , Layout
  , asStringList
  , createRoom
  , inBounds , outOfBounds
  , isPathableAt
  , getInteractableAt
  , terrainAt
) where

import Util
import Unit
import Object
import Player

import qualified Data.Map.Strict as Map
import GHC.Exts(sortWith)
import Data.List(transpose, sortBy, groupBy, foldl')
import Data.Ord(comparing)
import Data.Maybe(isJust, fromJust)
import Data.Either

data Level = Level {
    dim     :: XY
  , layout  :: Layout
  , units   :: Map.Map XY Unit
  , objects :: Map.Map XY Object
}


type Layout = Map.Map XY Glyph

type Dyna = Either Unit Object

terrainAt :: XY -> Level -> Glyph
terrainAt xy lvl = fromJust $ Map.lookup xy $ layout lvl

asStringList :: Layout -> [[Glyph]]
asStringList l = (\line -> (\(_,v) -> v) <$> line) <$> grouped where
    sorted  = sortWith (\((x,y),_) -> (y,x)) $ Map.toList l
    grouped = groupBy (\a b -> (snd $ fst a) == (snd $ fst b)) sorted

asMap :: XY -> [[Glyph]] -> Layout
asMap (x,y) strs = Map.fromList $ foldr (++) [] ll where
    ll = (\(s,x) -> ((\(c,y) -> ((x,y),c))
                 <$> zip s [0..y-1] )) <$> zip strs [0..x-1]

placeCorners :: XY -> Layout -> Layout
placeCorners (x,y) l = (Map.insert (0,0) (sym "DR"))    -- (0,0) is top left
                     $ (Map.insert (x,0) (sym "DL"))
                     $ (Map.insert (0,y) (sym "UR")) 
                     $  Map.insert (x,y) (sym "UL") l

placeWalls :: XY -> Layout -> Layout
placeWalls (x,y) l = l'' where
    top   = [(a,y) | a<-[0..x-1]]
    bot   = [(a,0) | a<-[0..x-1]]
    left  = [(0,b) | b<-[0..y-1]]
    right = [(x,b) | b<-[0..y-1]]
    l'  = foldl' (\m k -> Map.insert k (sym "H") m) l  (top++bot)
    l'' = foldl' (\m k -> Map.insert k (sym "V") m) l' (left++right)

enclose' :: XY -> Layout -> Layout
enclose' xy l = (placeCorners xy) $ placeWalls xy l

enclose :: [[Glyph]] -> [[Glyph]]   -- deprecated
enclose l = transpose l'' where
    dim = length $ l !! 0
    top = replicate dim (sym "H")
    bot = replicate dim (sym "H")
    l' = (init $ top : (tail l)) ++ [bot]
    left  = (sym "DR") : (replicate (dim-2) (sym "V")) ++ (sym "UR"):[]
    right = (sym "DL") : (replicate (dim-2) (sym "V")) ++ (sym "UL"):[]
    l'' = (init $ left : (tail $ transpose l')) ++ [right]

createRoom :: Int -> Level
createRoom seed = Level dim layout units objects where
    dim = (seed, seed)
    layout = enclose' dim $ asMap dim
        $ replicate (snd dim) $ replicate (fst dim) $ sym "dirt"
    units    = Map.fromList []
    objects  = Map.fromList []
             
outOfBounds :: XY -> XY -> Bool
outOfBounds (x,y) (w,h) =
    (x < 0) || (x > w-1) || (y < 0) || (y > h-1)
inBounds :: XY -> XY -> Bool
inBounds p d = not $ outOfBounds p d

isPathableAt :: XY -> Level -> Bool
isPathableAt xy lvl = 
    (inBounds xy (dim lvl)) && (isPathableGlyph $ terrainAt xy lvl)

getInteractableAt :: XY -> Level -> Maybe Dyna
getInteractableAt xy l =
    let mbU = Map.lookup xy $ units l
        mbO = Map.lookup xy $ objects  l
    in  if (isJust mbU) then Just $ Left $ fromJust mbU
                        else if (isJust mbO) then Just $ Right $ fromJust mbO
                                             else Nothing



