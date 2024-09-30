import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Data.List (sort)
import Sudoku (Board, parseBoard, rows, cols, boxes)

main :: IO ()
main = defaultMain tests

easyBoard :: Board
easyBoard = parseBoard
  [ "279..18.."
  , "6.3.5.927"
  , "5..297.36"
  , "95..2637."
  , "...83..92"
  , "832.74.1."
  , "4....3269"
  , ".9.7..5.1"
  , ".2568...3"
  ]

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "rows function idempotence" $
      (rows . rows) easyBoard @?= easyBoard
  , testCase "cols function idempotence" $
      (cols . cols) easyBoard @?= easyBoard
  , testCase "boxes function idempotence" $
      (boxes . boxes) easyBoard @?= easyBoard
  ]

propertyTests :: TestTree
propertyTests = testGroup "Properties"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  ]
