module Player ( 
    Player(..)
  , createPC
  , listDeities , listTraits
  , chooseDeity , chooseTraits
  , Str, Agi, Wil, Cha
  , Attrs(..)
  , Skills(..)
  , Deity(..)
  , Trait(..)
) where

import Util
import Unit
import Input

createPC :: Name -> Deity -> [Trait] -> Player
createPC na de tr = Player na 10 de st at sk tr su po where
    st = undefined
    at = undefined
    sk = undefined
    tr = []
    su = []
    po = (1,1)

data Player = Player {
    name   :: Name
  , faith  :: Int      
  , deity  :: Deity
  , stats  :: Stats     , attrs  :: Attrs
  , skills :: Skills    , traits :: [Trait]
  , status :: [Status]
  , pos    :: XY  
} deriving (Show)

data Attrs = Attrs {
    str :: Str  , agi :: Agi
  , wil :: Wil  , cha :: Cha
} deriving (Show, Eq, Ord)

type Str = Int
type Agi = Int
type Wil = Int
type Cha = Int

data Skills = Skills {
    glaive      :: Int
  , evocation   :: Int
  , invocation  :: Int
  , persuasion  :: Int
} deriving (Show, Eq, Ord)

data Deity = 
    TheShiningOne
  | Elyvilon
  | Zin
  deriving (Show, Eq, Ord, Enum, Read)

chooseDeity :: String -> Deity
chooseDeity s = toEnum 0 :: Deity

listDeities :: IO ()
listDeities = do
    let l = zip [1..] $ enumFrom (toEnum 0 :: Deity)
    mapM_ putStrLn $ (\(a,b)->show a++" "++show b) <$> l
listTraits :: IO ()
listTraits = do
    let l = zip [1..] $ enumFrom (toEnum 0 :: Trait)
    mapM_ putStrLn $ (\(a,b)->show a++" "++show b) <$> l

chooseTraits :: String -> [Trait]
chooseTraits s = (toEnum 0 :: Trait) : []

data Trait =
    BloodyMess
  | SexAppeal
  | Gifted
  | Skilled
  | GoodNatured
  | Jinxed
  deriving (Show, Eq, Ord, Enum, Read)
