module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars
import Data.Char
import Data.List.Split (splitOn)

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
validateFEN fenString = validRows && validRowLengths && validPieces
  where
    -- A FEN string looks like this: "/4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2/"
    rows = getRows fenString
    -- Check if there are 10 rows (or 8 at the start of the game)
    validRows = length rows == 10 || length rows == 8
    -- Check if each row has 10 cells
    validRowLengths = all (\row -> calculateRowLength row == 10) rows
    -- Check if the pieces are valid
    validPieces = all (all (`elem` "123456789bBwWgG")) rows

--- Helper functions

-- Calculate the length of a row in a FEN string

calculateRowLength :: String -> Int
calculateRowLength "" = 10
calculateRowLength row = sum $ map (\c -> if isDigit c then digitToInt c else 1) row

getRows :: String -> [String]
getRows = splitOn "/"

-- ##############################################################################
-- ################## IMPLEMENT buildBoard :: String -> Board ###################
-- ################## - 2 Functional Points                   ###################
-- ################## - 1 Coverage Point                      ###################
-- ##############################################################################

buildBoard :: String -> Board
buildBoard fenString = if length rows == 10 then buildFullBoard rows else buildInitialBoard rows
  where
    rows = getRows fenString

--- Helper functions

-- Build a full board from a FEN string
buildFullBoard :: [String] -> Board
buildFullBoard = map buildRow

-- Build an initial board from a FEN string
-- An initial board has 8 rows, with the first and last row being empty
buildInitialBoard :: [String] -> Board
buildInitialBoard rows = emptyRow : initialBoard ++ [emptyRow]
  where
    initialBoard = map buildRow rows
    emptyRow = replicate 10 Empty

--
buildRow :: String -> [Cell]
buildRow row =
  if row == ""
    then replicate 10 Empty
    else concatMap chunksFromFENCharacter row

chunksFromFENCharacter :: Char -> [Cell]
chunksFromFENCharacter c =
  if isDigit c
    then replicate (digitToInt c) Empty
    else case c of
      'b' -> replicate 1 (Soldier Black)
      'B' -> replicate 1 (Flag Black)
      'w' -> replicate 1 (Soldier White)
      'W' -> replicate 1 (Flag White)
      'g' -> replicate 1 (General White)
      'G' -> replicate 1 (General Black)
      _ -> []
