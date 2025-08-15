-- #############################################################################
-- ###### GRADING TESTS                                               ##########
-- #############################################################################

import Test.Hspec

import Board
  ( Board,
    Cell (Empty, Flag, General, Soldier),
    Dir (..),
    Player (Black, White),
    Pos (..),
    buildBoard,
    buildRow,
    calculateRowLength,
    chunksFromFENCharacter,
    getRows,
    validateFEN,
  )

import Catapult (Catapult (..), Move (..), catapultMoves, flagMoves, generalMoves, isEnemyPos, isGeneralNearCatapult, listMoves, playerWon, soldierMoves)

main :: IO ()
main = hspec $ do
  -- testBoards
  testValidateFENAll
  testBuildBoardAll
  testPos
  testShowInstances
  testEqInstances
  -- testCatapult
  testMoveDataType
  testCatapultDataType
  testFlagMoves
  testGeneralMoves
  testSoldierMoves
  testCatapultMoves
  testListMoves
  testPlayerWon
  testCatapultHelpers

-- #############################################################################
-- ###### Board TESTS                                                 ##########
-- #############################################################################

testValidateFENAll :: Spec
testValidateFENAll = describe "test FEN string validations" $ do
  testValidateFEN
  -- testing helper functions
  testCalculateRowLength
  testGetRows

testValidateFEN :: Spec
testValidateFEN = describe "test validateFEN" $ do
  it "empty string is not valid" $ do
    validateFEN "" `shouldBe` False
  it "initial board FEN with 8 rows is valid" $ do
    validateFEN "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/" `shouldBe` True
  it "full board FEN with 10 rows is valid" $ do
    validateFEN "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2" `shouldBe` True
  it "FEN with any other character than digits and pieces is not valid" $ do
    validateFEN "c3W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2" `shouldBe` False
  it "FEN with row length different than 10 is not valid" $ do
    validateFEN "4W4/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2" `shouldBe` False

testBuildBoardAll :: Spec
testBuildBoardAll = describe "test building Board" $ do
  testBuildBoard
  -- testing helper functions
  testBuildRow
  testChunksFromFENCharacter

testBuildBoard :: Spec
testBuildBoard = describe "test buildBoard" $ do
  it "build initial board" $ do
    buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
      `shouldBe` [ [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                   [Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White],
                   [Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White],
                   [Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White],
                   [Empty, Empty, Empty, Empty, Empty, General White, Empty, Empty, Empty, Empty],
                   [Empty, Empty, Empty, Empty, General Black, Empty, Empty, Empty, Empty, Empty],
                   [Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty],
                   [Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty],
                   [Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty],
                   [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                 ]
  it "build full board" $ do
    buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2"
      `shouldBe` [ [Empty, Empty, Empty, Empty, Flag White, Empty, Empty, Empty, Empty, Empty],
                   [Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White],
                   [Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White],
                   [Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White],
                   [Empty, Empty, Empty, Empty, Empty, General White, Empty, Empty, Empty, Empty],
                   [Empty, Empty, Empty, Empty, General Black, Empty, Empty, Empty, Empty, Empty],
                   [Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty],
                   [Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty],
                   [Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty, Soldier Black, Empty],
                   [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Flag Black, Empty, Empty]
                 ]

-- testing helper functions

-- calculateRowLength
testCalculateRowLength :: Spec
testCalculateRowLength = describe "test calculateRowLength" $ do
  it "calculate row length with digits" $ do
    calculateRowLength "1w1w1w1w1w" `shouldBe` 10
  it "calculate row length without digits" $ do
    calculateRowLength "wWwWwWwWwW" `shouldBe` 10
  it "calculate row length with 0" $ do
    calculateRowLength "0" `shouldBe` 0
  it "calculate row length only digits" $ do
    calculateRowLength "541" `shouldBe` 10
  it "calculate row length empty string" $ do
    calculateRowLength "" `shouldBe` 10

-- getRows
testGetRows :: Spec
testGetRows = describe "test getRows" $ do
  it "get rows" $ do
    getRows "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2"
      `shouldBe` ["4W5", "1w1w1w1w1w", "1w1w1w1w1w", "1w1w1w1w1w", "5g4", "4G5", "b1b1b1b1b1", "b1b1b1b1b1", "b1b1b1b1b1", "7B2"]
  it "get rows empty string" $ do
    getRows "" `shouldBe` [""]
  it "get rows only slashes" $ do
    getRows "//////" `shouldBe` ["", "", "", "", "", "", ""]

-- buildRow
testBuildRow :: Spec
testBuildRow = describe "test buildRow" $ do
  it "build row" $ do
    buildRow "1w1w1w1w1w" `shouldBe` [Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White, Empty, Soldier White]
  it "build row with digits" $ do
    buildRow "4W5" `shouldBe` [Empty, Empty, Empty, Empty, Flag White, Empty, Empty, Empty, Empty, Empty]
  it "build row with 10" $ do
    buildRow "10" `shouldBe` [Empty]
  it "build row with 0" $ do
    buildRow "0" `shouldBe` []
  it "build row only digits" $ do
    buildRow "541" `shouldBe` [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  it "build row empty string" $ do
    buildRow "" `shouldBe` [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  it "build row with invalid characters" $ do
    buildRow "fWwWwWwWwW" `shouldBe` [Flag White, Soldier White, Flag White, Soldier White, Flag White, Soldier White, Flag White, Soldier White, Flag White]

-- chunksFromFENCharacter
testChunksFromFENCharacter :: Spec
testChunksFromFENCharacter = describe "test chunksFromFENCharacter" $ do
  it "chunks from FEN digit character" $ do
    chunksFromFENCharacter '5' `shouldBe` [Empty, Empty, Empty, Empty, Empty]
  it "chunks from FEN piece character 'w'" $ do
    chunksFromFENCharacter 'w' `shouldBe` [Soldier White]
  it "chunks from FEN piece character 'W'" $ do
    chunksFromFENCharacter 'W' `shouldBe` [Flag White]
  it "chunks from FEN piece character 'g'" $ do
    chunksFromFENCharacter 'g' `shouldBe` [General White]
  it "chunks from FEN piece character 'G'" $ do
    chunksFromFENCharacter 'G' `shouldBe` [General Black]
  it "chunks from FEN piece character 'b'" $ do
    chunksFromFENCharacter 'b' `shouldBe` [Soldier Black]
  it "chunks from FEN piece character 'B'" $ do
    chunksFromFENCharacter 'B' `shouldBe` [Flag Black]
  it "chunks from invalid FEN character" $ do
    chunksFromFENCharacter 'c' `shouldBe` []

-- testing Pos type
testPos :: Spec
testPos = describe "test Pos type" $ do
  it "test Pos column and row" $ do
    let pos = Pos 'a' 2
    pos `shouldBe` Pos 'a' 2
  it "test Pos type functions" $ do
    let pos = Pos 'a' 2
    col pos `shouldBe` 'a'
    row pos `shouldBe` 2

-- testing auto derived types

-- Show instances
testShowInstances :: Spec
testShowInstances = describe "test Show instances" $ do
  testShowCell
  testShowPlayer
  testShowPos
  testShowDir
  testShowBoard

-- testing Show instance for Cell
testShowCell :: Spec
testShowCell = describe "test Show instance for Cell" $ do
  it "show Empty" $ do
    show Empty `shouldBe` "Empty"
  it "show Soldier White" $ do
    show (Soldier White) `shouldBe` "Soldier White"
  it "show Soldier Black" $ do
    show (Soldier Black) `shouldBe` "Soldier Black"
  it "show Flag White" $ do
    show (Flag White) `shouldBe` "Flag White"
  it "show Flag Black" $ do
    show (Flag Black) `shouldBe` "Flag Black"
  it "show General White" $ do
    show (General White) `shouldBe` "General White"
  it "show General Black" $ do
    show (General Black) `shouldBe` "General Black"

-- testing Show instance for Player
testShowPlayer :: Spec
testShowPlayer = describe "test Show instance for Player" $ do
  it "show White" $ do
    show White `shouldBe` "White"
    showList [White] "" `shouldBe` "[White]"
    showsPrec 0 White "" `shouldBe` "White"
  it "show Black" $ do
    show Black `shouldBe` "Black"
    showList [Black] "" `shouldBe` "[Black]"
    showsPrec 0 Black "" `shouldBe` "Black"

-- testing Show instance for Pos
testShowPos :: Spec
testShowPos = describe "test Show instance for Pos" $ do
  it "show Pos" $ do
    show (Pos 'a' 2) `shouldBe` "Pos {col = 'a', row = 2}"
    showList [Pos 'a' 2] "" `shouldBe` "[Pos {col = 'a', row = 2}]"
    showsPrec 0 (Pos 'a' 2) "" `shouldBe` "Pos {col = 'a', row = 2}"

-- testing Show instance for Dir
testShowDir :: Spec
testShowDir = describe "test Show instance for Dir" $ do
  it "show North" $ do
    show North `shouldBe` "North"
    showList [North] "" `shouldBe` "[North]"
    showsPrec 0 North "" `shouldBe` "North"
  it "show NorthEast" $ do
    show NorthEast `shouldBe` "NorthEast"
    showList [NorthEast] "" `shouldBe` "[NorthEast]"
    showsPrec 0 NorthEast "" `shouldBe` "NorthEast"
  it "show East" $ do
    show East `shouldBe` "East"
    showList [East] "" `shouldBe` "[East]"
    showsPrec 0 East "" `shouldBe` "East"
  it "show SouthEast" $ do
    show SouthEast `shouldBe` "SouthEast"
    showList [SouthEast] "" `shouldBe` "[SouthEast]"
    showsPrec 0 SouthEast "" `shouldBe` "SouthEast"
  it "show South" $ do
    show South `shouldBe` "South"
    showList [South] "" `shouldBe` "[South]"
    showsPrec 0 South "" `shouldBe` "South"
  it "show SouthWest" $ do
    show SouthWest `shouldBe` "SouthWest"
    showList [SouthWest] "" `shouldBe` "[SouthWest]"
    showsPrec 0 SouthWest "" `shouldBe` "SouthWest"
  it "show West" $ do
    show West `shouldBe` "West"
    showList [West] "" `shouldBe` "[West]"
    showsPrec 0 West "" `shouldBe` "West"
  it "show NorthWest" $ do
    show NorthWest `shouldBe` "NorthWest"
    showList [NorthWest] "" `shouldBe` "[NorthWest]"
    showsPrec 0 NorthWest "" `shouldBe` "NorthWest"

-- testing Show instance for Board
testShowBoard :: Spec
testShowBoard = describe "test Show instance for Board" $ do
  it "show Board" $ do
    show [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]] `shouldBe` "[[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]]"
  it "show Board with pieces" $ do
    show [[Soldier White, Empty, Soldier Black], [Empty, Flag White, Empty], [General Black, Empty, General White]] `shouldBe` "[[Soldier White,Empty,Soldier Black],[Empty,Flag White,Empty],[General Black,Empty,General White]]"
  it "show empty Board" $ do
    show ([] :: Board) `shouldBe` "[]"
  it "show empty rows Board" $ do
    show ([[]] :: Board) `shouldBe` "[[]]"

-- testing Eq instances
testEqInstances :: Spec
testEqInstances = describe "test Eq instances" $ do
  testEqPos
  testEqPlayer
  testEqCell

-- testing Eq instance for Pos
testEqPos :: Spec
testEqPos = describe "test Eq instance for Pos" $ do
  it "Pos equal" $ do
    Pos 'a' 2 == Pos 'a' 2 `shouldBe` True
  it "Pos not equal columns" $ do
    Pos 'a' 2 /= Pos 'b' 2 `shouldBe` True
  it "Pos not equal rows" $ do
    Pos 'a' 2 /= Pos 'a' 3 `shouldBe` True

-- testing Eq instance for Player
testEqPlayer :: Spec
testEqPlayer = describe "test Eq instance for Player" $ do
  it "White equal" $ do
    White == White `shouldBe` True
  it "Black equal" $ do
    Black == Black `shouldBe` True
  it "White not equal Black" $ do
    White /= Black `shouldBe` True

-- testing Eq instance for Cell
testEqCell :: Spec
testEqCell = describe "test Eq instance for Cell" $ do
  it "Empty equal" $ do
    Empty == Empty `shouldBe` True
  it "Soldier equal" $ do
    Soldier White == Soldier White `shouldBe` True
    Soldier Black == Soldier Black `shouldBe` True
  it "Flag equal" $ do
    Flag White == Flag White `shouldBe` True
    Flag Black == Flag Black `shouldBe` True
  it "General equal" $ do
    General White == General White `shouldBe` True
    General Black == General Black `shouldBe` True
  it "Empty not equal Soldier" $ do
    Empty /= Soldier White `shouldBe` True
  it "Soldier not equal Flag" $ do
    Soldier White /= Flag White `shouldBe` True
  it "Flag not equal General" $ do
    Flag White /= General White `shouldBe` True
  it "General not equal Empty" $ do
    General White /= Empty `shouldBe` True

-- #############################################################################
-- ###### CATAPULT TESTS                                              ##########
-- #############################################################################

-- flagMoves
testFlagMoves :: Spec
testFlagMoves = describe "test flagMoves" $ do
  let initialBoard = buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"
  it "flag moves for White" $ do
    flagMoves initialBoard White `shouldBe` [Move (Pos 'b' 9) (Pos 'b' 9), Move (Pos 'c' 9) (Pos 'c' 9), Move (Pos 'd' 9) (Pos 'd' 9), Move (Pos 'e' 9) (Pos 'e' 9), Move (Pos 'f' 9) (Pos 'f' 9), Move (Pos 'g' 9) (Pos 'g' 9), Move (Pos 'h' 9) (Pos 'h' 9), Move (Pos 'i' 9) (Pos 'i' 9)]
  it "flag moves for Black" $ do
    flagMoves initialBoard Black `shouldBe` [Move (Pos 'b' 0) (Pos 'b' 0), Move (Pos 'c' 0) (Pos 'c' 0), Move (Pos 'd' 0) (Pos 'd' 0), Move (Pos 'e' 0) (Pos 'e' 0), Move (Pos 'f' 0) (Pos 'f' 0), Move (Pos 'g' 0) (Pos 'g' 0), Move (Pos 'h' 0) (Pos 'h' 0), Move (Pos 'i' 0) (Pos 'i' 0)]

-- Move
testMoveDataType :: Spec
testMoveDataType = describe "tests for Move data type" $ do
  describe "Show instance for Move" $ do
    it "show Move" $ do
      show (Move (Pos 'a' 2) (Pos 'b' 3)) `shouldBe` "a2-b3"
    it "showList Move" $ do
      showList [Move (Pos 'a' 2) (Pos 'b' 3)] "" `shouldBe` "[a2-b3]"
    it "showsPrec Move" $ do
      showsPrec 0 (Move (Pos 'a' 2) (Pos 'b' 3)) "" `shouldBe` "a2-b3"
  describe "Eq instance for Move" $ do
    it "Move equal" $ do
      Move (Pos 'a' 2) (Pos 'b' 3) == Move (Pos 'a' 2) (Pos 'b' 3) `shouldBe` True
    it "Move not equal" $ do
      Move (Pos 'a' 2) (Pos 'b' 3) /= Move (Pos 'a' 2) (Pos 'b' 2) `shouldBe` True
  describe "Move functions" $ do
    it "Move.start" $ do
      start (Move (Pos 'a' 2) (Pos 'b' 3)) `shouldBe` Pos 'a' 2
    it "Move.target" $ do
      target (Move (Pos 'a' 2) (Pos 'b' 3)) `shouldBe` Pos 'b' 3

-- Catapult
testCatapultDataType :: Spec
testCatapultDataType = describe "tests for Catapult data type" $ do
  describe "Show instance for Catapult" $ do
    it "show Catapult" $ do
      show N `shouldBe` "N"
      show NE `shouldBe` "NE"
      show E `shouldBe` "E"
      show SE `shouldBe` "SE"
      show S `shouldBe` "S"
      show SW `shouldBe` "SW"
      show W `shouldBe` "W"
      show NW `shouldBe` "NW"
    it "showList Catapult" $ do
      showList [N, NE, E, SE, S, SW, W, NW] "" `shouldBe` "[N,NE,E,SE,S,SW,W,NW]"
    it "showsPrec Catapult" $ do
      showsPrec 0 N "" `shouldBe` "N"
      showsPrec 0 NE "" `shouldBe` "NE"
      showsPrec 0 E "" `shouldBe` "E"
      showsPrec 0 SE "" `shouldBe` "SE"
      showsPrec 0 S "" `shouldBe` "S"
      showsPrec 0 SW "" `shouldBe` "SW"
      showsPrec 0 W "" `shouldBe` "W"
      showsPrec 0 NW "" `shouldBe` "NW"

{-
Game Board

    a     b     c     d     e     f     g     h     i     j
  ___________________________________________________________
 |     |     |     |     |     |     |     |     |     |     |
9|     |     |     |     |  W  |     |     |     |     |     |
 |_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
 |     |     |     |     |     |     |     |     |     |     |
8|     |  w  |     |  w  |     |     |     |  w  |     |  w  |
 |_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
 |     |     |     |     |     |     |     |     |     |     |
7|     |  w  |     |  w  |     |     |  w  |  w  |     |  w  |
 |_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
 |     |     |     |     |     |     |     |     |     |     |
6|     |  w  |     |     |     |  g  |  w  |     |     |  w  |
 |_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
 |     |     |     |     |     |     |     |     |     |     |
5|     |     |     |  w  |     |     |  w  |     |     |     |
 |_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
 |     |     |     |     |     |     |     |     |     |     |
4|     |     |     |     |  G  |     |     |  w  |     |     |
 |_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
 |     |     |     |     |     |     |     |     |     |     |
3|  b  |     |  b  |     |  b  |     |  b  |     |  b  |     |
 |_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
 |     |     |     |     |     |     |     |     |     |     |
2|  b  |     |  b  |     |  b  |     |  b  |     |  b  |     |
 |_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
 |     |     |     |     |     |     |     |     |     |     |
1|  b  |     |  b  |     |  b  |     |  b  |     |  b  |     |
 |_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|
 |     |     |     |     |     |     |     |     |     |     |
0|     |     |     |     |     |     |     |  B  |     |     |
 |_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|

 -}
gameBoard :: Board
gameBoard = buildBoard "4W5/1w1w21w1w/1w1w2ww1w/1w111gw11w/3w11w3/4G2w2/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2"

initialBoard :: Board
initialBoard = buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/"

-- generalMoves
testGeneralMoves :: Spec
testGeneralMoves = describe "test generalMoves" $ do
  it "general moves for White at f6" $ do
    generalMoves gameBoard White (Pos 'f' 6) `shouldBe` [Move (Pos 'f' 6) (Pos 'e' 5), Move (Pos 'f' 6) (Pos 'e' 6), Move (Pos 'f' 6) (Pos 'e' 7), Move (Pos 'f' 6) (Pos 'f' 5), Move (Pos 'f' 6) (Pos 'f' 7)]
  it "general moves for Black at e4" $ do
    generalMoves gameBoard Black (Pos 'e' 4) `shouldBe` [Move (Pos 'e' 4) (Pos 'd' 3), Move (Pos 'e' 4) (Pos 'd' 4), Move (Pos 'e' 4) (Pos 'e' 5), Move (Pos 'e' 4) (Pos 'f' 3), Move (Pos 'e' 4) (Pos 'f' 4), Move (Pos 'e' 4) (Pos 'f' 5)]
  it "general moves for invalid pos" $ do
    generalMoves gameBoard White (Pos 'e' 4) `shouldBe` []
    generalMoves gameBoard Black (Pos 'e' 5) `shouldBe` []

-- soldierMoves
testSoldierMoves :: Spec
testSoldierMoves = describe "test soldierMoves" $ do
  it "soldier moves for White at d7" $ do
    soldierMoves gameBoard White (Pos 'd' 7) `shouldBe` [Move (Pos 'd' 7) (Pos 'c' 7), Move (Pos 'd' 7) (Pos 'd' 6), Move (Pos 'd' 7) (Pos 'e' 7)]
  it "soldier moves for Black at e2" $ do
    soldierMoves gameBoard Black (Pos 'e' 2) `shouldBe` [Move (Pos 'e' 2) (Pos 'd' 2), Move (Pos 'e' 2) (Pos 'f' 2)]
  it "soldier moves for invalid pos" $ do
    soldierMoves gameBoard White (Pos 'e' 3) `shouldBe` []
    soldierMoves gameBoard Black (Pos 'e' 6) `shouldBe` []
  it "soldier moves for White at h4" $ do
    soldierMoves gameBoard White (Pos 'h' 4) `shouldBe` [Move (Pos 'h' 4) (Pos 'g' 4), Move (Pos 'h' 4) (Pos 'h' 3), Move (Pos 'h' 4) (Pos 'i' 4), Move (Pos 'h' 4) (Pos 'g' 3), Move (Pos 'h' 4) (Pos 'i' 3), Move (Pos 'h' 4) (Pos 'h' 6)]
  it "soldier moves for Black at i3" $ do
    soldierMoves gameBoard Black (Pos 'i' 3) `shouldBe` [Move (Pos 'i' 3) (Pos 'h' 3), Move (Pos 'i' 3) (Pos 'i' 4), Move (Pos 'i' 3) (Pos 'j' 3), Move (Pos 'i' 3) (Pos 'h' 4)]
  it "soldier moves for White at d5" $ do
    soldierMoves gameBoard White (Pos 'd' 5) `shouldBe` [Move (Pos 'd' 5) (Pos 'c' 5), Move (Pos 'd' 5) (Pos 'd' 4), Move (Pos 'd' 5) (Pos 'e' 5), Move (Pos 'd' 5) (Pos 'e' 4)]

-- catapultMoves
testCatapultMoves :: Spec
testCatapultMoves = describe "test catapultMoves" $ do
  it "catapult moves for White at g7" $ do
    catapultMoves gameBoard White (Pos 'g' 7) `shouldBe` [Move (Pos 'g' 7) (Pos 'g' 3), Move (Pos 'g' 7) (Pos 'g' 2), Move (Pos 'g' 7) (Pos 'g' 4)]
  it "catapult moves for White at g5" $ do
    catapultMoves gameBoard White (Pos 'g' 5) `shouldBe` [Move (Pos 'g' 5) (Pos 'g' 8)]
  it "catapult moves for non-catapult piece" $ do
    catapultMoves gameBoard White (Pos 'd' 7) `shouldBe` []
    catapultMoves gameBoard White (Pos 'e' 3) `shouldBe` []
    catapultMoves gameBoard White (Pos 'e' 4) `shouldBe` []
  it "catapult moves without general" $ do
    catapultMoves gameBoard White (Pos 'b' 8) `shouldBe` [Move (Pos 'b' 8) (Pos 'b' 5)]
  it "incomplete catapult move" $ do
    catapultMoves gameBoard White (Pos 'd' 8) `shouldBe` []

-- listMoves
testListMoves :: Spec
testListMoves = describe "test listMoves" $ do
  it "list moves for White" $ do
    listMoves gameBoard White `shouldBe` [Move (Pos 'f' 6) (Pos 'e' 5), Move (Pos 'f' 6) (Pos 'e' 6), Move (Pos 'f' 6) (Pos 'e' 7), Move (Pos 'f' 6) (Pos 'f' 5), Move (Pos 'f' 6) (Pos 'f' 7), Move (Pos 'b' 6) (Pos 'a' 6), Move (Pos 'b' 6) (Pos 'b' 5), Move (Pos 'b' 6) (Pos 'c' 6), Move (Pos 'b' 7) (Pos 'a' 7), Move (Pos 'b' 7) (Pos 'c' 7), Move (Pos 'b' 8) (Pos 'a' 8), Move (Pos 'b' 8) (Pos 'c' 8), Move (Pos 'd' 5) (Pos 'c' 5), Move (Pos 'd' 5) (Pos 'd' 4), Move (Pos 'd' 5) (Pos 'e' 5), Move (Pos 'd' 5) (Pos 'e' 4), Move (Pos 'd' 7) (Pos 'c' 7), Move (Pos 'd' 7) (Pos 'd' 6), Move (Pos 'd' 7) (Pos 'e' 7), Move (Pos 'd' 8) (Pos 'c' 8), Move (Pos 'd' 8) (Pos 'e' 8), Move (Pos 'g' 5) (Pos 'f' 5), Move (Pos 'g' 5) (Pos 'g' 4), Move (Pos 'g' 5) (Pos 'h' 5), Move (Pos 'g' 6) (Pos 'h' 6), Move (Pos 'g' 7) (Pos 'f' 7), Move (Pos 'h' 4) (Pos 'g' 4), Move (Pos 'h' 4) (Pos 'h' 3), Move (Pos 'h' 4) (Pos 'i' 4), Move (Pos 'h' 4) (Pos 'g' 3), Move (Pos 'h' 4) (Pos 'i' 3), Move (Pos 'h' 4) (Pos 'h' 6), Move (Pos 'h' 7) (Pos 'h' 6), Move (Pos 'h' 7) (Pos 'i' 7), Move (Pos 'h' 8) (Pos 'g' 8), Move (Pos 'h' 8) (Pos 'i' 8), Move (Pos 'j' 6) (Pos 'i' 6), Move (Pos 'j' 6) (Pos 'j' 5), Move (Pos 'j' 7) (Pos 'i' 7), Move (Pos 'j' 8) (Pos 'i' 8), Move (Pos 'b' 6) (Pos 'b' 9), Move (Pos 'b' 8) (Pos 'b' 5), Move (Pos 'g' 5) (Pos 'g' 8), Move (Pos 'g' 7) (Pos 'g' 3), Move (Pos 'g' 7) (Pos 'g' 2), Move (Pos 'g' 7) (Pos 'g' 4), Move (Pos 'j' 6) (Pos 'j' 9), Move (Pos 'j' 8) (Pos 'j' 5)]
  it "list moves for Black" $ do
    listMoves gameBoard Black `shouldBe` [Move (Pos 'e' 4) (Pos 'd' 3), Move (Pos 'e' 4) (Pos 'd' 4), Move (Pos 'e' 4) (Pos 'e' 5), Move (Pos 'e' 4) (Pos 'f' 3), Move (Pos 'e' 4) (Pos 'f' 4), Move (Pos 'e' 4) (Pos 'f' 5), Move (Pos 'a' 1) (Pos 'b' 1), Move (Pos 'a' 2) (Pos 'b' 2), Move (Pos 'a' 3) (Pos 'a' 4), Move (Pos 'a' 3) (Pos 'b' 3), Move (Pos 'c' 1) (Pos 'b' 1), Move (Pos 'c' 1) (Pos 'd' 1), Move (Pos 'c' 2) (Pos 'b' 2), Move (Pos 'c' 2) (Pos 'd' 2), Move (Pos 'c' 3) (Pos 'b' 3), Move (Pos 'c' 3) (Pos 'c' 4), Move (Pos 'c' 3) (Pos 'd' 3), Move (Pos 'e' 1) (Pos 'd' 1), Move (Pos 'e' 1) (Pos 'f' 1), Move (Pos 'e' 2) (Pos 'd' 2), Move (Pos 'e' 2) (Pos 'f' 2), Move (Pos 'e' 3) (Pos 'd' 3), Move (Pos 'e' 3) (Pos 'f' 3), Move (Pos 'g' 1) (Pos 'f' 1), Move (Pos 'g' 1) (Pos 'h' 1), Move (Pos 'g' 2) (Pos 'f' 2), Move (Pos 'g' 2) (Pos 'h' 2), Move (Pos 'g' 3) (Pos 'f' 3), Move (Pos 'g' 3) (Pos 'g' 4), Move (Pos 'g' 3) (Pos 'h' 3), Move (Pos 'g' 3) (Pos 'h' 4), Move (Pos 'i' 1) (Pos 'h' 1), Move (Pos 'i' 1) (Pos 'j' 1), Move (Pos 'i' 2) (Pos 'h' 2), Move (Pos 'i' 2) (Pos 'j' 2), Move (Pos 'i' 3) (Pos 'h' 3), Move (Pos 'i' 3) (Pos 'i' 4), Move (Pos 'i' 3) (Pos 'j' 3), Move (Pos 'i' 3) (Pos 'h' 4), Move (Pos 'a' 1) (Pos 'a' 4), Move (Pos 'a' 3) (Pos 'a' 0), Move (Pos 'c' 1) (Pos 'c' 4), Move (Pos 'c' 3) (Pos 'c' 0), Move (Pos 'e' 3) (Pos 'e' 0), Move (Pos 'g' 1) (Pos 'g' 4), Move (Pos 'g' 3) (Pos 'g' 0), Move (Pos 'i' 1) (Pos 'i' 4), Move (Pos 'i' 3) (Pos 'i' 0)]
  it "list moves for initial board for white" $ do
    listMoves initialBoard White `shouldBe` [Move (Pos 'b' 9) (Pos 'b' 9), Move (Pos 'c' 9) (Pos 'c' 9), Move (Pos 'd' 9) (Pos 'd' 9), Move (Pos 'e' 9) (Pos 'e' 9), Move (Pos 'f' 9) (Pos 'f' 9), Move (Pos 'g' 9) (Pos 'g' 9), Move (Pos 'h' 9) (Pos 'h' 9), Move (Pos 'i' 9) (Pos 'i' 9)]
  it "list moves for initial board for black" $ do
    listMoves initialBoard Black `shouldBe` [Move (Pos 'b' 0) (Pos 'b' 0), Move (Pos 'c' 0) (Pos 'c' 0), Move (Pos 'd' 0) (Pos 'd' 0), Move (Pos 'e' 0) (Pos 'e' 0), Move (Pos 'f' 0) (Pos 'f' 0), Move (Pos 'g' 0) (Pos 'g' 0), Move (Pos 'h' 0) (Pos 'h' 0), Move (Pos 'i' 0) (Pos 'i' 0)]
  it "list moves for board without general" $ do
    listMoves (buildBoard "4W5/111www1w1w/1w1w1w1w1w/1w1wgw1w1w//////7B2") Black `shouldBe` []

-- playerWon
testPlayerWon :: Spec
testPlayerWon = describe "test playerWon" $ do
  it "flag captured by White" $ do
    playerWon (buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1") White `shouldBe` True
  it "black's general doesn't exist" $ do
    playerWon (buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4//b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2") White `shouldBe` True
  it "black doen't have any valid moves" $ do
    playerWon (buildBoard "4W5/111www1w1w/1w1wGw1w1w/1w1wgw1w1w//////7B2") White `shouldBe` True

-- test catapult helper functions
testCatapultHelpers :: Spec
testCatapultHelpers = describe "test helper functions in Catapult" $ do
  it "test isEnemyPos" $ do
    isEnemyPos gameBoard White (Pos 'h' 0) `shouldBe` True
    isEnemyPos gameBoard White (Pos 'g' 5) `shouldBe` False
    isEnemyPos gameBoard Black (Pos 'f' 6) `shouldBe` True
    isEnemyPos gameBoard Black (Pos 'e' 2) `shouldBe` False
  it "test isGeneralNearCatapult" $ do
    isGeneralNearCatapult gameBoard White (Pos 'g' 7, Pos 'g' 6, Pos 'g' 5) `shouldBe` True
    isGeneralNearCatapult gameBoard White (Pos 'f' 5, Pos 'f' 4, Pos 'f' 3) `shouldBe` True
