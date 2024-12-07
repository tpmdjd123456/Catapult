-- #############################################################################
-- ###### GRADING TESTS                                               ##########
-- #############################################################################

import Test.Hspec

import Board
     (validateFEN,
      buildBoard,
      Player(White, Black),
      Cell(Empty, Flag, Soldier, General),
      Pos(Pos))

import Catapult (Move(Move), playerWon, flagMoves, generalMoves, soldierMoves, catapultMoves, listMoves)

main :: IO ()
main = putStrLn "implement your tests here"