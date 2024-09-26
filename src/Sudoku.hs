module Sudoku(parseBoard,Board,Row,Cell,CellValue,validBoard) where

import Data.Char (digitToInt)
import Data.List (transpose,nub)

type Board = [Row]
type Row = [Cell]
data Cell = Empty
          | WithValue CellValue
          deriving (Eq)

data CellValue = One
               | Two
               | Three
               | Four
               | Five
               | Six
               | Seven
               | Eight
               | Nine
               deriving (Eq)

type ExtractRows = Board -> [Row]

boardWidth :: Int
boardWidth = 9

instance Show CellValue where
  show One      = "1"
  show Two      = "2"
  show Three    = "3"
  show Four     = "4"
  show Five     = "5"
  show Six      = "6"
  show Seven    = "7"
  show Eight    = "8"
  show Nine     = "9"

instance Show Cell where
  show Empty = "."
  show (WithValue v) = show v

allValues :: [CellValue]
allValues = [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

parseCell :: Char -> Cell
parseCell '.' = Empty
parseCell digit = WithValue (allValues !! (digitToInt digit - 1))

parseRow :: String -> Row
parseRow = map parseCell

parseBoard :: [String] -> Board
parseBoard = map parseRow

rows :: Board -> [Row]
rows = id

cols :: Board -> [Row]
cols = transpose

boxes :: Board -> [Row]
boxes boardRows = 
    [[boardRows !! y !! x | y <- [3*incY .. 3*incY + 2], x <- [3*incX .. 3*incX + 2] ] | incX <- [0,1,2], incY <- [0,1,2]]

hasValue :: Cell -> Bool
hasValue (WithValue _) = True
hasValue Empty = False

validRow :: Row -> Bool
validRow = (== boardWidth) . length . nub . filter hasValue

validateRows :: Board -> ExtractRows -> Bool
validateRows board extractRows = (all validRow . extractRows) board

validBoard :: Board -> Bool
validBoard board = all (validateRows board) [rows,cols,boxes]