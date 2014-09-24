module Sudoku where

import Test.QuickCheck

-------------------------------------------------------------------------

data Sudoku = Sudoku [[Maybe Int]]
 deriving ( Show, Eq )

rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 row)
    where row = replicate 9 Nothing

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = and [and b | b = [length (rows s) == 9, length r == 9]]
    where 
        r = [row | row <- (rows s)]
        p = [pos | pos <- r]

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved = undefined

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku = undefined

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku = undefined

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = undefined

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-------------------------------------------------------------------------
