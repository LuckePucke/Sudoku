module Sudoku where

import Test.QuickCheck
import Data.Char

-------------------------------------------------------------------------

data Sudoku = Sudoku [[Maybe Int]]
 deriving ( Show, Eq )

rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

example :: Sudoku
example = Sudoku
    [ 
        [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing],
        [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing],
        [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing],
        [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8],
        [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9],
        [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing],
        [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing],
        [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing],
        [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]

exampleFalse :: Sudoku
exampleFalse = Sudoku
    [ 
        [Just 10, Just 0, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing],
        [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing],
        [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing],
        [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8],
        [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9],
        [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing],
        [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing],
        [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing],
        [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 row)
    where row = replicate 9 Nothing

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = and [
                  b | r <- [row | row <- (rows s)],
                      p <- [pos | pos <- r],
                      b <- [
                            length (rows s) == 9,
                            length r == 9,
                            isOK p
                           ]
                 ]

isOK :: Maybe Int -> Bool
isOK Nothing = True
isOK (Just n) | n < 1     = False
              | n > 9     = False
              | otherwise = True

isNotNothing :: Maybe Int -> Bool
isNotNothing Nothing = False
isNotNothing _       = True

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved s = and [
                  b | r <- [row | row <- (rows s)],
                      p <- [pos | pos <- r],
                      b <- [isNotNothing p]
                 ]

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku m) = printHelp m []

printHelp :: [[Maybe Int]] -> String -> IO ()
printHelp [[]]             s = putStr (reverse('\n':s))
printHelp ([]:l)           s = printHelp l ('\n':s)
printHelp ((Nothing:r):l)  s = printHelp (r:l) ('.':s)
printHelp (((Just n):r):l) s = printHelp (r:l) ((intToDigit n):s)

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = 
    do  content <- readFile path
        let rows = lines content
        let sudo = Sudoku (readHelp rows)
        if isSudoku sudo
            then return sudo
            else error "readSudoku: File doesn't contain a sudoku."

readHelp :: [String] -> [[Maybe Int]]
readHelp []     = []
readHelp (s:ss) = (readHelp' s) : readHelp ss

readHelp' :: String -> [Maybe Int]
readHelp' ""      = []
readHelp' ('.':s) = Nothing : readHelp' s
readHelp' (c:s)   = Just i  : readHelp' s
    where i = digitToInt c

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency
    [
        (9, return Nothing),
        (1, do i <- choose (1,9)
               return (Just i))
    ]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
-- Why not just test quickCheck isSudoku?

-------------------------------------------------------------------------

type Block = [Maybe Int]

-- isOkayBlock checks if 
isOkayBlock :: Block -> Bool
isOkayBlock block = blockOK block []

blockOK :: Block -> [Maybe Int] -> Bool
blockOK []           _ = True
blockOK (Nothing:as) b = blockOK as b
blockOK (a:as) b
    | equalsAny a b = False
    | otherwise     = blockOK as (a:b)

equalsAny :: Eq a => a -> [a] -> Bool
equalsAny _ [] = False
equalsAny a (b:bs)
    | a == b    = True
    | otherwise = equalsAny a bs

-- Combines 3 lists making a longer list
-- [a] -> [b] -> [c] -> [a, b, c]
tripend :: Ord a => [a] -> [a] -> [a] -> [a]
tripend a b c = append (append (append (append a []) b) []) c

-- Help function for tripend
append :: Ord a => [a] -> [a] -> [a]
append []     b = b
append (a:as) b = append as (a:b)

blocks :: Sudoku -> [Block]
blocks s = tripend (rows s) (columns (rows s) 0) (squares (rows s) 0 0)

prop_blocks :: Sudoku -> Bool
prop_blocks s = length bs == 27 && and [length a == 9 | a <- bs]
    where
        bs = blocks s

columns :: [Block] -> Int -> [Block]
columns _    9 = []
columns rows n = column rows n : columns rows (n+1)

column :: [Block] -> Int -> Block
column []     _ = []
column (r:rs) n = r!!n : column rs n

squares :: [Block] -> Int -> Int -> [Block]
squares r 9 6 = []
squares r 9 y = square r 0 (y+3) : squares r 3 (y+3)
squares r x y = square r x y     : squares r (x+3) y

square :: [Block] -> Int -> Int -> Block
square r x y = 
    tripend (take 3 (drop x (r!!y1))) (take 3 (drop x (r!!y2))) (take 3 (drop x (r!!y3)))
        where
            y1 = y
            y2 = y + 1
            y3 = y + 2


