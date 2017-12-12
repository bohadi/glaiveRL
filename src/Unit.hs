module Unit ( 
    Name
  , Hp, St, Mp
  , Stats(..)
  , Status(..)
  , Unit(..)
) where

import Util

type Name = String
type Hp = Int
type St = Int
type Mp = Int

data Stats = Stats {
    hp :: Hp    , maxhp :: Hp
  , st :: St    , maxst :: St
  , mp :: Mp    , maxmp :: Mp
} deriving (Show, Eq, Ord)

data Status =
    Poison
  | Berserk
  | Muddle
  deriving (Show, Eq, Ord)
   
type Unit = (Name, Stats, XY)

type BeastA = Unit
mkBeastA :: Int -> BeastA
mkBeastA seed = ("beastA", (Stats 2 2 4 4 0 0), (4,seed))
type BeastB = Unit
mkBeastB :: Int -> BeastB
mkBeastB seed = ("beastB", (Stats 4 4 2 2 0 0), (4,seed))
type BeastC = Unit
mkBeastC :: Int -> BeastC
mkBeastC seed = ("beastC", (Stats 4 4 4 4 0 0), (4,seed))

type Serpentling = Unit
mkSerpentling :: Int -> Serpentling
mkSerpentling seed = ("serpentling", (Stats 2 2 3 3 0 0), (2,seed))
type Serpent = Unit
mkSerpent :: Int -> Serpent
mkSerpent seed = ("serpent", (Stats 3 3 5 5 0 0), (2,seed))

type Spiderling = Unit
mkSpiderling :: Int -> Serpentling
mkSpiderling seed = ("spiderling", (Stats 2 2 1 1 0 0), (2,seed))
type Spider = Unit
mkSpider :: Int -> Spider
mkSpider seed = ("spider", (Stats 4 4 3 3 0 0), (2,seed))

type Zomling = Unit
mkZomling :: Int -> Zomling
mkZomling seed = ("zomling", (Stats 3 3 1 1 0 0), (6,seed))
type Zombie = Unit
mkZombie :: Int -> Zombie
mkZombie seed = ("zombie", (Stats 4 4 2 2 0 0), (6,seed))
