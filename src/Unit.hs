module Unit ( 
    Stats(..)
  , Status(..)
  , Unit(..)
  , ustat
) where

import Util

import qualified Data.Map.Strict as Map
import Data.Maybe(fromJust)

data Stats = Stats {
    hp :: Int   , maxhp :: Int
  , st :: Int   , maxst :: Int
  , mp :: Int   , maxmp :: Int
} deriving (Show, Eq, Ord)

data Status =
    Poison
  | Berserk
  | Muddle
  deriving (Show, Eq, Ord)
   
type Unit = (Name, GlyphID, Stats)

ustat :: GlyphID -> Stats
ustat u = fromJust $ Map.lookup u unitStats
unitStats = Map.fromList [
    ("beastA"     , Stats 2 2 4 4 0 0 )
  , ("beastB"     , Stats 4 4 2 2 0 0 )
  , ("beastC"     , Stats 4 4 4 4 0 0 )
  ]
