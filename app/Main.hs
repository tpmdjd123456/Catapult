module Main (main) where

import System.Random
import System.Environment

import Catapult (listMoves)
import Board (buildBoard, Player (White, Black))

main :: IO ()
main = do
    args <- getArgs
    let (fen:p:_) = args
        player = if p == "w" then White else Black
        moves = listMoves (buildBoard fen) player in
                putStrLn (show moves)

