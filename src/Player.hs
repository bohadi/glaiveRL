module Player ( 
    Player
  , createPC
  , listDeities , listTraits
  , chooseDeity , chooseTraits
  , Deity(..)
  , Trait(..)
) where

import Util
import Unit

data Player = Player {
    name   :: Name
  , faith  :: Int      
  , deity  :: Deity
  , stats  :: Stats     , attrs  :: Attrs
  , skills :: Skills    , traits :: [Trait]
  , status :: [Status]
  , pos    :: XY  
} deriving (Show)

data Deity = 
    TheShiningOne
  | Elyvilon
  | Zin
  deriving (Show, Eq, Ord, Enum, Read)

data Trait =
    BloodyMess
  | SexAppeal
  | Gifted
  | Skilled
  | GoodNatured
  | Jinxed
  deriving (Show, Eq, Ord, Enum, Read)

listDeities :: IO ()
listDeities = do
    let l = zip [1..] $ enumFrom (toEnum 0 :: Deity)
    mapM_ putStrLn $ (\(a,b)->show a++" "++show b) <$> l
listTraits :: IO ()
listTraits = do
    let l = zip [1..] $ enumFrom (toEnum 0 :: Trait)
    mapM_ putStrLn $ (\(a,b)->show a++" "++show b) <$> l

createPC :: Name -> Deity -> [Trait] -> Player
createPC na de tr = Player na 10 de st at sk tr su po where
    st = undefined
    at = undefined
    sk = undefined
    tr = []
    su = []
    po = (1,1)

chooseDeity :: String -> Deity
chooseDeity s = toEnum 0 :: Deity

chooseTraits :: String -> [Trait]
chooseTraits s = (toEnum 0 :: Trait) : []
