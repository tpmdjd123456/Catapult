module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars
import Data.Char

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = White | Black deriving Show
data Cell = Empty | General Player | Soldier Player | Flag Player deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
data Dir = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) White White = True
  (==) Black Black = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Soldier p1) (Soldier p2) = p1 == p2
  (==) (General p1) (General p2) = p1 == p2
  (==) (Flag p1) (Flag p2) = p1 == p2
  (==) _ _ = False


-- ##############################################################################
-- ################## IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                   ###################
-- ################## - 1 Coverage Point                      ###################
-- #############################################################################

validateFEN :: String -> Bool
validateFEN _ = True


-- ##############################################################################
-- ################## IMPLEMENT buildBoard :: String -> Board ###################
-- ################## - 2 Functional Points                   ###################
-- ################## - 1 Coverage Point                      ###################
-- ##############################################################################

buildBoard :: String -> Board
buildBoard _ = []