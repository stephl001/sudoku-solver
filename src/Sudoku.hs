module Sudoku
  ( parseBoard
  , rows
  , cols
  , boxes
  , Board
  , Row
  , Cell
  , CellValue
  , validBoard
  ) where

import           Data.Char       (digitToInt)
import           Data.List       (intersect, nub, subsequences, transpose, (\\))
import           Data.List.Split (chunksOf)
import           Data.Maybe      (mapMaybe)

type Board = [Row]

type Row = [Cell]

type Cell = [CellValue]

type IndexedCellValues = (CellValue, [Int])

data CellValue
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Enum, Bounded)

type ExtractRows = Board -> [Row]

boardWidth :: Int
boardWidth = 9

instance Show CellValue where
  show One   = "1"
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"

parseBoard :: [String] -> Board
parseBoard = map parseRow

allValues :: [CellValue]
allValues = [minBound .. maxBound]

parseCell :: Char -> Cell
parseCell '.'   = allValues
parseCell digit = [allValues !! (digitToInt digit - 1)]

parseRow :: String -> Row
parseRow = map parseCell

rows :: Board -> [Row]
rows = id

cols :: Board -> [Row]
cols = transpose

boxes :: Board -> [Row]
boxes = map concat . concatMap transpose . chunksOf 3 . map (chunksOf 3)

hasSingleValue :: Cell -> Bool
hasSingleValue [_] = True
hasSingleValue _   = False

validRow :: Row -> Bool
validRow row = all hasSingleValue row && distinctValues
  where
    distinctValues = (length . nub . concat) row == boardWidth

validateRows :: Board -> ExtractRows -> Bool
validateRows board extractRows = (all validRow . extractRows) board

validBoard :: Board -> Bool
validBoard board = all (validateRows board) [rows, cols, boxes]

easyBoard1 :: Board
easyBoard1 =
  parseBoard
    [ "86.7951.4"
    , "45.....9."
    , "37.18.652"
    , ".8....3.1"
    , "...9.128."
    , "..5.3...."
    , "..85.3..6"
    , ".9641...."
    , ".1.....2."
    ]

hardBoard1 :: Board
hardBoard1 =
  parseBoard
    [ ".6..4...."
    , "8..7.6..."
    , ".....1..2"
    , ".5....231"
    , "7431.2..5"
    , ".91.3..6."
    , "...9..72."
    , "......5.9"
    , "17...43.."
    ]

expertBoard1 :: Board
expertBoard1 =
  parseBoard
    [ "5....2..."
    , ".42...8.7"
    , ".9......."
    , "......43."
    , "8..9.42.."
    , "4736...81"
    , ".8....7.9"
    , ".3...9.12"
    , "219.7...."
    ]

-- Cannot be solved!
extremeBoard1 :: Board
extremeBoard1 =
  parseBoard
    [ "........2"
    , "..7...56."
    , ".83......"
    , "..81...3."
    , "7....8.1."
    , "...47..8."
    , "34...6..."
    , ".....5..."
    , "2.6.9..7."
    ]

-- Function to find all naked groups of a given size in a row
findNakedGroups :: Int -> Row -> [[Cell]]
findNakedGroups size row =
  [ group
  | group <- subsequences row
  , length group == size
  , allEqual group
  , length (head group) == size
  ]

-- Function to find all hidden groups of a given size in a row
findHiddenGroups :: Int -> Row -> [[CellValue]]
findHiddenGroups size row =
  [ group
  | group <- subsequences [minBound .. maxBound]
  , length group == size
  , countOccurrences group row == size
  ]

-- Helper function to check if all elements in a list are equal
allEqual :: Eq a => [a] -> Bool
allEqual (x:xs) = all (== x) xs
allEqual []     = True

-- Helper function to count occurrences of a group in a row
countOccurrences :: [CellValue] -> Row -> Int
countOccurrences group row =
  length [cell | cell <- row, not (null (cell `intersect` group))]

-- Function to prune a row based on naked and hidden groups
pruneRowSized :: Int -> Row -> Row
pruneRowSized size row =
  foldl pruneCell (foldl pruneCellHidden row hiddenGroups) nakedGroups
  where
    nakedGroups = nub $ findNakedGroups size row
    hiddenGroups = nub $ findHiddenGroups size row
    pruneCell r group =
      map
        (\cell ->
           if cell `notElem` group
             then cell \\ (head group)
             else cell)
        r
    pruneCellHidden r group =
      map
        (\cell ->
           if not (null (cell `intersect` group)) && length cell > size
             then cell `intersect` group
             else cell)
        r

pruneRow :: Int -> Row -> Row
pruneRow maxSize row = foldl (flip pruneRowSized) row [1 .. maxSize]

sampleRow :: Row
sampleRow =
  [[Eight], [Six], allValues, [Seven], [Nine], [Five], [One], allValues, [Four]]

nakedPairsRow :: Row
nakedPairsRow =
  [ [One, Three, Seven]
  , [Two]
  , [Five]
  , [Three, Six, Eight]
  , [Three, Four, Eight]
  , [One, Seven]
  , [Three, Four, Six]
  , [Nine]
  , [One, Seven]
  ]

hiddenPairsRow :: Row
hiddenPairsRow =
  [ [Three, Eight]
  , [Two]
  , [Three, Six]
  , [Three, Four, Six]
  , [Five]
  , [Three, Four]
  , [One, Seven, Eight]
  , [Nine]
  , [One, Six, Seven]
  ]

nakedTripsRow :: Row
nakedTripsRow =
  [ [One, Four, Seven]
  , [One, Seven, Eight]
  , [Two]
  , [Five]
  , [Three, Six, Eight]
  , [One, Seven, Eight]
  , [Nine]
  , [Three, Four, Six]
  , [One, Seven, Eight]
  ]

solve :: Board -> [Board]
solve = filter validBoard . candidates

candidates :: Board -> [Board]
candidates b = error "Must implement!"

-- -- Prunes recursively until the board cannot be simplified anymore...
pruneBoard :: Int -> Board -> Board
pruneBoard maxSubgroupSize board =
  let newBoard =
        foldr (pruneTransformedBoard maxSubgroupSize) board [rows, cols, boxes]
   in if newBoard == board
        then board
        else pruneBoard maxSubgroupSize newBoard

-- pruneBoardDebug :: Board -> [Board]
-- pruneBoardDebug board =
--   let newBoard = foldr pruneTransformedBoard board [rows,cols,boxes]
--   in if newBoard == board then [board] else board : pruneBoardDebug newBoard
pruneTransformedBoard :: Int -> ExtractRows -> Board -> Board
pruneTransformedBoard maxSubgroupSize extractRows =
  (extractRows . (pruneRows maxSubgroupSize) . extractRows)

pruneRows :: Int -> [Row] -> [Row]
pruneRows maxSubgroupSize = map (pruneRow maxSubgroupSize)
