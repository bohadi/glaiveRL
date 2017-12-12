module Glaive (
    start
) where

import Prelude hiding(interact)

import Util
import Unit
import Object
import Player
import Level
import World
import Draw
import Input

import Data.Maybe(isJust, fromJust)
import qualified Data.Either as E
import Control.Lens

type Game = (Player, World)

start :: IO ()
start = do
    g  <- newGame
    gg <- gameLoop g
    s  <- score gg
    return ()

gameOver :: Game -> Bool
gameOver (p,w) = False

score :: Game -> IO ()
score (p,w) = do
    return ()

newGame :: IO (Player, World)
newGame = do 
    p <- newPlayer
    w <- newWorld
    return (p,w)

newPlayer :: IO Player
newPlayer = do
    putStrLn "Your namesake:"
    name <- getLine
    putStrLn "Your Deity:"
    listDeities
    deity <- getLine
    putStrLn "You may choose 0-2 traits."
    listTraits
    traits <- getLine
    return $ createPC name (chooseDeity deity) (chooseTraits traits)

newWorld :: IO World
newWorld = do
    return $ World [] [("cur", createRoom 20)]

gameLoop :: Game -> IO Game
gameLoop g@(p,w) = do 
    render p w
    i <- getInput
    if i == (Cmd Quit) then return g else
        let g' = update g i
        in if gameOver g'
           then do putStrLn "Good game."
                   return g'
           else gameLoop g'

isValid :: Game -> Input -> Bool
isValid (p,w) i = True

update :: Game -> Input -> Game
update (p,w) (Move i) =
  let lvl  = currentLevel w
      pos' = move (p ^. pos) i
      isP  = isPathableAt      pos' lvl
      mbI  = getInteractableAt pos' lvl
      p'   = p & pos .~ pos'  
  in if (isJust mbI)
        then interact (p,w) pos'
        else if isP then (p',w) else (p,w)

interact :: Game -> XY -> Game
interact g@(p,w) xy =
    let i = fromJust $ getInteractableAt xy $ currentLevel w
    in case i of
         E.Left  u -> interactWithUnit   g u
         E.Right o -> interactWithObject g o
                 
interactWithUnit :: Game -> Unit -> Game
interactWithUnit g u = g

interactWithObject :: Game -> Object -> Game
interactWithObject g o = g
        

