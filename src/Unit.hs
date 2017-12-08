module Unit ( 
    Name
  , Hp, St, Mp
  , Str, Agi, Wil, Cha
  , Stats(..)
  , Attrs(..)
  , Skills(..)
  , Status(..)
) where

import Util

type Name = String
type Hp = Int
type St = Int
type Mp = Int
type Str = Int
type Agi = Int
type Wil = Int
type Cha = Int

data Stats = Stats {
    hp :: Hp    , maxhp :: Hp
  , st :: St    , maxst :: St
  , mp :: Mp    , maxmp :: Mp
} deriving (Show, Eq, Ord)

data Attrs = Attrs {
    str :: Str  , agi :: Agi
  , wil :: Wil  , cha :: Cha
} deriving (Show, Eq, Ord)

data Skills = Skills {
    glaive      :: Int
  , evocation   :: Int
  , invocation  :: Int
  , persuasion  :: Int
} deriving (Show, Eq, Ord)

data Status =
    Poison
  | Berserk
  | Muddle
  deriving (Show, Eq, Ord)
   
