module Main where

import Glaive(start)

import System.Environment

main :: IO ()
main = do
    putStrLn "\t ┏━━━━━━━━━━━━┓"
    putStrLn "\t ┃ GlaiveRL ₩ ┃"
    putStrLn "\t ┗━━━━━━━━━━━━┛"
    putStrLn ""
    putStrLn "\t 1 new game"
    putStrLn "\t 2 do nothing"
    putStrLn "\t _ quit"
    putStrLn "\n\n\n\n\n"
    cmd <- getLine
    if cmd == "1"
        then start
        else putStrLn "Bye."


