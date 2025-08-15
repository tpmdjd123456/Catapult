module Catapult where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Board
import Data.Char
import Data.Maybe

data Move = Move {start :: Pos, target :: Pos}

instance Show Move where
  show (Move (Pos startC startR) (Pos targetC targetR)) = [startC] ++ show startR ++ "-" ++ [targetC] ++ show targetR

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1)) (Move (Pos sc2 sr2) (Pos tc2 tr2)) =
    sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2

-- #################################################################################################
-- ################## IMPLEMENT flagMoves :: Board -> Player -> [Move]           ###################
-- ################## - 2 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

data Catapult = N | NE | E | SE | S | SW | W | NW deriving Show

flagMoves :: Board -> Player -> [Move]
flagMoves board player =
  case player of
    White ->
      if head board == emptyRow
        then map (`flagMove` 9) validFlagColumns
        else []
    Black ->
      if last board == emptyRow
        then map (`flagMove` 0) validFlagColumns
        else []
  where
    emptyRow = replicate 10 Empty
    validFlagColumns = ['b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']
    flagMove c r = Move (Pos c r) (Pos c r)

-- #################################################################################################
-- ################## IMPLEMENT generalMoves :: Board -> Player -> Pos -> [Move] ###################
-- ################## - 4 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

generalMoves :: Board -> Player -> Pos -> [Move]
generalMoves board player pos =
  case cell of
    General p ->
      if p == player
        then map (Move pos) validSurroundingPositions
        else []
    _ -> []
  where
    cell = getCell board pos
    surroundingPositions = getSurroundingPositions pos
    validSurroundingPositions = filter (\pos -> getCell board pos == Empty) surroundingPositions

--- Helper functions

-- Get the Cell at a given position
getCell :: Board -> Pos -> Cell
getCell board (Pos col row) = (board !! rowIndex) !! (ord col - ord 'a')
  where
    rowIndex = 9 - row

-- Get the surrounding positions of a given position
getSurroundingPositions :: Pos -> [Pos]
getSurroundingPositions (Pos col row) =
  filter isValidPos allPos
  where
    allPos = [Pos (chr (ord col + x)) (row + y) | x <- [-1, 0, 1], y <- [-1, 0, 1], x /= 0 || y /= 0]

-- Check if a position is valid
isValidPos :: Pos -> Bool
isValidPos (Pos col row) = col >= 'a' && col <= 'j' && row >= 0 && row <= 9

-- #################################################################################################
-- ################## IMPLEMENT soldierMoves :: Board -> Player -> Pos -> [Move] ###################
-- ################## - 4 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

soldierMoves :: Board -> Player -> Pos -> [Move]
soldierMoves board player pos =
  case cell of
    Soldier p ->
      if p == player
        then normal_moves ++ attackMoves ++ retreat_moves
        else []
    _ -> []
  where
    cell = getCell board pos
    (normal_pos, all_attack_pos, all_retreat_pos) = getAllFinalSoldierPos player pos
    normal_moves = map (Move pos) $ filter (\pos -> getCell board pos == Empty) normal_pos
    attackMoves = map (Move pos) $ filter (isEnemyPos board player) all_attack_pos
    underAttack = any (\pos -> getCell board pos == Soldier (enemyPlayer player)) all_attack_pos
    interveningPos = getRetreatPos player pos 1
    retreatPath = zip interveningPos all_retreat_pos
    retreat_moves =
      if underAttack
        then
          mapMaybe
            ( \(interveningPos, retreatPos) ->
                if getCell board interveningPos == Empty && getCell board retreatPos == Empty
                  then Just (Move pos retreatPos)
                  else Nothing
            )
            retreatPath
        else []

--- Helper functions

-- Get possible moves for a soldier at pos
getAllFinalSoldierPos :: Player -> Pos -> ([Pos], [Pos], [Pos])
getAllFinalSoldierPos player pos =
  (normal_pos, all_attack_pos, all_retreat_pos)
  where
    normal_pos = getNormalPos player pos
    all_attack_pos = getAttackPos player pos
    all_retreat_pos = getRetreatPos player pos 2

-- Get normal move Pos for a soldier at pos
getNormalPos :: Player -> Pos -> [Pos]
getNormalPos player (Pos col row) =
  filter isValidPos normal_pos
  where
    normal_pos =
      case player of
        White -> [Pos (chr (ord col - 1)) row, Pos col (row - 1), Pos (chr (ord col + 1)) row]
        Black -> [Pos (chr (ord col - 1)) row, Pos col (row + 1), Pos (chr (ord col + 1)) row]

-- Get attack move Pos for a soldier at pos
getAttackPos :: Player -> Pos -> [Pos]
getAttackPos player (Pos col row) =
  filter isValidPos all_attack_pos
  where
    possible_attack_cols = [chr (ord col - 1), col, chr (ord col + 1)]
    all_attack_pos =
      case player of
        White -> map (\c -> Pos c (row - 1)) possible_attack_cols
        Black -> map (\c -> Pos c (row + 1)) possible_attack_cols

-- Get retreat move Pos for a soldier which can retreat n steps (diagonally or straight)
getRetreatPos :: Player -> Pos -> Int -> [Pos]
getRetreatPos player (Pos col row) n =
  filter isValidPos all_retreat_pos
  where
    possible_retreat_cols = [chr (ord col - n), col, chr (ord col + n)]
    all_retreat_pos =
      case player of
        White -> map (\c -> Pos c (row + n)) possible_retreat_cols
        Black -> map (\c -> Pos c (row - n)) possible_retreat_cols

enemyPlayer :: Player -> Player
enemyPlayer White = Black
enemyPlayer Black = White

isEnemyPos :: Board -> Player -> Pos -> Bool
isEnemyPos board player pos =
  isEnemyOccupied player $ getCell board pos

-- #################################################################################################
-- ################## IMPLEMENT catapultMoves :: Board -> Player -> Pos -> [Move]  ###################
-- ################## - 4 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

catapultMoves :: Board -> Player -> Pos -> [Move]
catapultMoves board player pos =
  case cell of
    Soldier p ->
      if p == player
        then catapultFireMoves ++ catapultNormalMoves
        else []
    _ -> []
  where
    cell = getCell board pos
    allCatapults = getCatapults board player pos
    catapultFireMoves = concatMap (getCatapultFireMoves player board) allCatapults
    catapultNormalMoves = mapMaybe (getCatapultNormalMove board) allCatapults

getCatapultFireMoves :: Player -> Board -> (Catapult, (Pos, Pos, Pos)) -> [Move]
getCatapultFireMoves player board (catapult, p@(p1, p2, p3)) = catMaybes [fire1Move, fire2Move]
  where
    fire1 = getCatapultPos p3 catapult 2
    fire2 = getCatapultPos p3 catapult 3
    fire1Move =
      if isValidPos fire1 && isGeneralNearCatapult board player p && isEnemyOccupied player (getCell board fire1)
        then Just (Move p1 fire1)
        else Nothing
    fire2Move =
      if isValidPos fire2 && isGeneralNearCatapult board player p && isEnemyOccupied player (getCell board fire2)
        then Just (Move p1 fire2)
        else Nothing

getCatapultNormalMove :: Board -> (Catapult, (Pos, Pos, Pos)) -> Maybe Move
getCatapultNormalMove board (catapult, (p1, p2, p3)) =
  if isValidPos movePos && getCell board movePos == Empty
    then Just (Move p1 movePos)
    else Nothing
  where
    movePos = getCatapultPos p3 catapult 1

isEnemyOccupied :: Player -> Cell -> Bool
isEnemyOccupied player cell =
  case cell of
    Soldier p -> p == enemyPlayer player
    General p -> p == enemyPlayer player
    Flag p -> p == enemyPlayer player
    _ -> False

isGeneralNearby :: Board -> Player -> Pos -> Bool
isGeneralNearby board player pos =
  any (\p -> getCell board p == General player) (getSurroundingPositions pos)

-- Check if the general is near the catapult. The general is near the catapult if it can move to the catapult in one
-- step Catapult is at p1, p2, p3. General is near the catapult if it can move to p1 or p3 as if general can move to p2,
-- it can move to p1 or p3 as well
isGeneralNearCatapult :: Board -> Player -> (Pos, Pos, Pos) -> Bool
isGeneralNearCatapult board player (p1, _, p3) =
  isGeneralNearby board player p1 || isGeneralNearby board player p3

getCatapults :: Board -> Player -> Pos -> [(Catapult, (Pos, Pos, Pos))]
getCatapults board player pos =
  filter (\(d, (p1, p2, p3)) -> getCell board p1 == Soldier player && getCell board p2 == Soldier player && getCell board p3 == Soldier player) validPossibleCatapults
  where
    allPossibleCatapultDirections = [N, NE, E, SE, S, SW, W, NW]
    allPossibleCatapults = map (getAllCatapultMembersPos pos) allPossibleCatapultDirections
    validPossibleCatapults = filter (\(_, (p1, p2, p3)) -> isValidPos p1 && isValidPos p2 && isValidPos p3) $ zip allPossibleCatapultDirections allPossibleCatapults

getAllCatapultMembersPos :: Pos -> Catapult -> (Pos, Pos, Pos)
getAllCatapultMembersPos pos catapult = (pos, getCatapultPos pos catapult 1, getCatapultPos pos catapult 2)

getCatapultPos :: Pos -> Catapult -> Int -> Pos
getCatapultPos (Pos col row) N n = Pos (col) (row + n)
getCatapultPos (Pos col row) NE n = Pos (chr (ord col + n)) (row + n)
getCatapultPos (Pos col row) E n = Pos (chr (ord col + n)) (row)
getCatapultPos (Pos col row) SE n = Pos (chr (ord col + n)) (row - n)
getCatapultPos (Pos col row) S n = Pos (col) (row - n)
getCatapultPos (Pos col row) SW n = Pos (chr (ord col - n)) (row - n)
getCatapultPos (Pos col row) W n = Pos (chr (ord col - n)) (row)
getCatapultPos (Pos col row) NW n = Pos (chr (ord col - n)) (row + n)

-- #################################################################################################
-- ################## IMPLEMENT playerWon :: Board -> Maybe Player               ###################
-- ################## - 2 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

playerWon :: Board -> Player -> Bool
playerWon board player = not opponentFlagExists || not opponentGeneralExists || not opponentHasValidMoves
  where
    opponentFlagExists = flagExists board (enemyPlayer player)
    opponentGeneralExists = generalExists board (enemyPlayer player)
    opponentHasValidMoves = listMoves board (enemyPlayer player) /= []

flagExists :: Board -> Player -> Bool
flagExists board player = any (any (\cell -> cell == Flag player)) board

generalExists :: Board -> Player -> Bool
generalExists board player = any (any (\cell -> cell == General player)) board

-- #################################################################################################
-- ################## IMPLEMENT listMoves :: Board -> Player -> [Move]           ###################
-- ################## - 2 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

listMoves :: Board -> Player -> [Move]
listMoves board player =
  if flagMove /= []
    then flagMove
    else generalMove ++ soldierMove ++ catapultMove
  where
    flagMove = flagMoves board player
    ourGeneralPos = getGeneralPos board player
    generalMove = maybe [] (generalMoves board player) ourGeneralPos
    ourSoldierPositions = getSoldierPositions board player
    soldierMove = concatMap (soldierMoves board player) ourSoldierPositions
    catapultMove = concatMap (catapultMoves board player) ourSoldierPositions

getGeneralPos :: Board -> Player -> Maybe Pos
getGeneralPos board player =
  listToMaybe $
    mapMaybe
      ( \(col, row) ->
          if getCell board (Pos col row) == General player
            then Just (Pos col row)
            else Nothing
      )
      allPositions
  where
    allPositions = [(col, row) | col <- ['a' .. 'j'], row <- [0 .. 9]]

getSoldierPositions :: Board -> Player -> [Pos]
getSoldierPositions board player =
  mapMaybe
    ( \(col, row) ->
        if getCell board (Pos col row) == Soldier player
          then Just (Pos col row)
          else Nothing
    )
    allPositions
  where
    allPositions = [(col, row) | col <- ['a' .. 'j'], row <- [0 .. 9]]
