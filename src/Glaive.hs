module Glaive (
    start
) where

import Util
import Unit
import Player
import Level
import World
import Draw
import Input

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
  let lvl = currentLevel w
      pos' = move (pos p) i
      isI  = isInteractable pos' lvl
      isP  = isPathable     pos' lvl
      p'   = p { pos = pos' }
      -- p'   = p & pos ~ pos'   -- lens style
  in if isP then (p',w) else (p,w)





