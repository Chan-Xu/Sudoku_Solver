-- Name: Chan Xu
-- UID: u6233112
-- Collaborators: Jiafan Zhang(u6334938)
--                Zixun Wu(u6193344)
module Sudoku
  ( allBlanks
  , isSudoku
  , noBlanks
  , printSudoku
  , fromString
  , toString
  , rows
  , cols
  , boxs
  , okBlock
  , okSudoku
  , blank
  , (!!=)
  , update
  , solve
  ) where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.List.Split

-- A matrix is a list of rows.
type Matrix a = [Row a]

-- A row is a list of values
type Row a = [a]

-- A Sudoku puzzle is a matrix of cells
newtype Sudoku =
  Sudoku (Matrix Cell)
  deriving (Show, Eq)

-- | cells extracts the cells from a Sudoku
cells (Sudoku m) = m

-- Each cell may contain a number from 1 to 9, or nothing
type Cell = Maybe Int

example :: Sudoku
example =
  Sudoku
    [ [ Just 3
      , Just 6
      , Nothing
      , Nothing
      , Just 7
      , Just 1
      , Just 2
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Just 5
      , Nothing
      , Nothing
      , Nothing
      , Nothing
      , Just 1
      , Just 8
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Just 9
      , Just 2
      , Nothing
      , Just 4
      , Just 7
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Nothing
      , Nothing
      , Just 1
      , Just 3
      , Nothing
      , Just 2
      , Just 8
      ]
    , [ Just 4
      , Nothing
      , Nothing
      , Just 5
      , Nothing
      , Just 2
      , Nothing
      , Nothing
      , Just 9
      ]
    , [ Just 2
      , Just 7
      , Nothing
      , Just 4
      , Just 6
      , Nothing
      , Nothing
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Just 5
      , Just 3
      , Nothing
      , Just 8
      , Just 9
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Just 8
      , Just 3
      , Nothing
      , Nothing
      , Nothing
      , Nothing
      , Just 6
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Just 7
      , Just 6
      , Just 9
      , Nothing
      , Nothing
      , Just 4
      , Just 3
      ]
    ]

-- allBlanks is a Sudoku with just blanks
allBlanks :: Sudoku
allBlanks = Sudoku ( replicate 9 ( replicate 9 Nothing ))


-- | isSudoku checks if a Sudoku has the proper dimensions
-- >>> isSudoku (Sudoku [])
-- False
-- >>> isSudoku allBlanks
-- True
-- >>> isSudoku example
-- True
-- >>> isSudoku (Sudoku (tail (cells example)))
-- False

-- Jiafan Zhang told me that I can use 'cells' function from line 36.
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku s) = length s == 9 && length (cells(Sudoku s)) == 9


-- | noBlanks checks if a Sudoku has no blanks
-- >>> noBlanks example
-- False
noBlanks :: Sudoku -> Bool
noBlanks (Sudoku s) = notElem Nothing (concat s)

-- | printSudoku prints a Sudoku as a 9 x 9 grid
-- >>>printSudoku example
--    3 6 . . 7 1 2 . .
--    . 5 . . . . 1 8 .
--    . . 9 2 . 4 7 . .
--    . . . . 1 3 . 2 8
--    4 . . 5 . 2 . . 9
--    2 7 . 4 6 . . . .
--    . . 5 3 . 8 9 . .
--    . 8 3 . . . . 6 .
--    . . 7 6 9 . . 4 3
printSudoku :: Sudoku -> IO ()
--printSudoku s = putStr (toString s)
printSudoku s = putStr $ unlines $ chunksOf 9 (toString s)

-- | cell generates an arbitrary cell in a Sudoku
-- The frequency of Nothing versus Just n values is currently 90% versus 10%,
-- but you may want to change that ratio.
cell :: Gen (Maybe Int)
cell =
  frequency
    [(10, oneof [return (Just n) | n <- [1 .. 9]]), (90, return Nothing)]

-- | An instance for generating Arbitrary Sudokus
-- prop> isSudoku s
instance Arbitrary Sudoku where
  arbitrary = do
    rows <- sequence [sequence [cell | j <- [1 .. 9]] | i <- [1 .. 9]]
    return (Sudoku rows)

-- | fromString converts an 81-character canonical string encoding for a
-- | Sudoku into our internal representation
fromString :: String -> Sudoku
--fromString str = Sudoku [map charToInt x | x <- lines str]
fromString str = Sudoku (chunksOf 9 (map charToInt str))
  where
    charToInt '.' = Nothing
    charToInt  c  = Just (ord c-48)


-- | toString converts a Sudoku into its canonical 81-character string
-- | encoding
-- prop> fromString (toString s) == s
toString :: Sudoku -> String
--toString s = unlines [ map intToChar y | y <- cells s]
toString s = map intToChar (concat(cells s))
  where
    intToChar Nothing  = '.'
    intToChar (Just i) = chr (i+48)

type Block a = [a]

-- Extract the blocks of rows
rows :: Matrix a -> [Block a]
rows m = m

-- Extract the blocks of columns
cols :: Matrix a -> [Block a]
cols = transpose

-- Extract the blocks of 3*3 boxes
-- This is a wrong code I wrote below.
 {- boxs [] = []
    boxs m = box1 (take 3 m) ++ box1 (drop 3 m)
      where
        box1 [[],[],[]] = []
        box1 x = concat (map (take 3) x) : box1 (map (drop 3) x)
 -}
-- >>> boxs (cells example)
{- [[Just 3,Just 6,Nothing,Nothing,Just 5,Nothing,Nothing,Nothing,Just 9],
    [Nothing,Nothing,Nothing,Just 4,Nothing,Nothing,Just 2,Just 7,Nothing],
    [Nothing,Nothing,Just 5,Nothing,Just 8,Just 3,Nothing,Nothing,Just 7],
    [Nothing,Just 7,Just 1,Nothing,Nothing,Nothing,Just 2,Nothing,Just 4],
    [Nothing,Just 1,Just 3,Just 5,Nothing,Just 2,Just 4,Just 6,Nothing],
    [Just 3,Nothing,Just 8,Nothing,Nothing,Nothing,Just 6,Just 9,Nothing],
    [Just 2,Nothing,Nothing,Just 1,Just 8,Nothing,Just 7,Nothing,Nothing],
    [Nothing,Just 2,Just 8,Nothing,Nothing,Just 9,Nothing,Nothing,Nothing],
    [Just 9,Nothing,Nothing,Nothing,Just 6,Nothing,Nothing,Just 4,Just 3]]
-}
-- reference Zixun Wu (u6193344)
boxs :: Matrix a -> [Block a]
boxs matrix = map concat [(map (take 3 . drop i) . take 3 . drop j) matrix | i <- [0, 3, 6], j <- [0, 3, 6]]

-- | Test if a block of cells does not contain the same integer twice
-- >>> okBlock [Just 1, Just 7, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 2]
-- True
-- >>> okBlock [Just 1, Just 7, Nothing, Just 7, Just 3, Nothing, Nothing, Nothing, Just 2]
-- False
okBlock :: Block Cell -> Bool
okBlock []           = True
okBlock (x:xs)       = case x of
  Nothing -> okBlock xs
  _       -> notElem x xs && okBlock xs

-- | No block contains the same integer twice
-- >>> okSudoku allBlanks
-- True
-- >>> okSudoku $ fromString "36..712...5....18...92.47......13.284..5.2..927.46......53.89...83....6...769..43"
-- True
-- >>> okSudoku $ fromString "364871295752936184819254736596713428431582679278469351645328917983147562127695843"
-- True
okSudoku :: Sudoku -> Bool
okSudoku (Sudoku m) = all okBlock (rows m ++ cols m ++ boxs m)

type Pos = (Int, Int)

-- | Return a blank position in the Sudoku
-- >>> blank allBlanks
-- (0,0)
-- >>> blank example
-- (0,2)
blank :: Sudoku -> Pos
blank s = head [ (y,x) | y <- [0 .. 8], x <- [0 .. 8], index (cells s) (y,x) == Nothing]

-- It's a helper function indexing for Sudoku.
index :: Matrix a -> Pos -> a
index list (y,x) = (list !! y) !! x

-- | Given a list, and a tuple containing an index in the list and a new value,
-- | update the given list with the new value at the given index.
-- >>> ["a","b","c","d"] !!= (1,"apa")
-- ["a","apa","c","d"]
-- >>> ["p","qq","rrr"] !!= (0,"bepa")
-- ["bepa","qq","rrr"]
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) list (i, a) = take i list ++ [a] ++ drop (i+1) list

-- | Given a Sudoku, a position, and a new cell value,
-- | update the given Sudoku at the given position with the new value.
update :: Sudoku -> Pos -> Int -> Sudoku
update (Sudoku s) (y,x) i = Sudoku ((!!=) s (y,n))
  where
    n = (!!=) (s !! y) (x,Just i)

-- | solve takes an 81-character encoding of a Sudoku puzzle and returns a
-- | list of solutions for it, if any
solve :: String -> [String]
solve s
  | not (okSudoku sud) = []
  | noBlanks sud       = [s]
  | otherwise          = concat (map (solve . toString) updateSudoku)
      where
        updateSudoku = map (update sud (blank sud)) [1 .. 9]
        sud          = fromString s

-------------------------------------------TESTING----------------------------------------
-- | Test the blank function
-- >>>quickCheck prop_blank
-- +++ OK, passed 100 tests.
prop_blank :: Sudoku -> Bool
prop_blank su@(Sudoku matrix) = (matrix !! (fst (blank su))) !! (snd (blank su)) == Nothing

-- | Test the isSudoku function
-- >>>quickCheck prop_isSudoku
-- +++ OK, passed 100 tests.
prop_isSudoku= isSudoku


