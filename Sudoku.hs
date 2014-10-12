module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.Maybe

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
        [Just 10, Just 1, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing],
        [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing],
        [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing],
        [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8],
        [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9],
        [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing],
        [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing],
        [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing],
        [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]



-- Part I

-------------------------------------------------------------------------

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

-- Help function for isSudoku
-- Makes sure a Maybe Int represents a cell in a sudoku.
-- Nothing | Just 1 | ... | Just 9
isOK :: Maybe Int -> Bool
isOK Nothing = True
isOK (Just n) | n < 1     = False
              | n > 9     = False
              | otherwise = True

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved s = and [
                  b | r <- [row | row <- (rows s)],
                      p <- [pos | pos <- r],
                      b <- [isNotNothing p]
                 ]
                 
-- Help function for isSolved
isNotNothing :: Maybe Int -> Bool
isNotNothing Nothing = False
isNotNothing _       = True

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



-- Part II

-------------------------------------------------------------------------

type Block = [Maybe Int]

-- isOkayBlock returns true if the block does not contain the same digit
-- twice.
isOkayBlock :: Block -> Bool
isOkayBlock block = blockOK block []

-- Help function for isOkayBlock
blockOK :: Block -> [Maybe Int] -> Bool
blockOK []           _ = True
blockOK (Nothing:as) b = blockOK as b
blockOK (a:as) b
    | equalsAny a b = False
    | otherwise     = blockOK as (a:b)

-- Help function for blockOK. (And therefore an indirect help function for
-- isOkayBlock.
equalsAny :: Eq a => a -> [a] -> Bool
equalsAny _ [] = False
equalsAny a (b:bs)
    | a == b    = True
    | otherwise = equalsAny a bs

-- Combines 3 lists making a longer list
-- Will be used for a few functions.
-- [a] -> [b] -> [c] -> [a, b, c]
tripend :: Ord a => [a] -> [a] -> [a] -> [a]
tripend a b c = append (append (append (append a []) b) []) c

-- Help function for tripend
append :: Ord a => [a] -> [a] -> [a]
append []     b = b
append (a:as) b = append as (a:b)

-- Returns all blocks in a sudoku as a list of blocks.
-- Uses tripend to combine lists of all 3 types. (square, row, column.)
blocks :: Sudoku -> [Block]
blocks s = tripend (rows s) (columns (rows s) 0) (squares (rows s) 0 0)

prop_blocks :: Sudoku -> Bool
prop_blocks s = length bs == 27 && and [length a == 9 | a <- bs]
    where
        bs = blocks s

-- Help function for blocks
columns :: [Block] -> Int -> [Block]
columns _    9 = []
columns rows n = column rows n : columns rows (n+1)

-- Help function for columns (And therefore an indirect help function for
-- blocks)
column :: [Block] -> Int -> Block
column []     _ = []
column (r:rs) n = r!!n : column rs n

-- Help function for blocks
squares :: [Block] -> Int -> Int -> [Block]
squares r 9 6 = []
squares r 9 y = square r 0 (y+3) : squares r 3 (y+3)
squares r x y = square r x y     : squares r (x+3) y

-- Help function for squares (And therefore an indirect help function for
-- blocks)
square :: [Block] -> Int -> Int -> Block
square r x y = 
    tripend (take 3 (drop x (r!!y1))) (take 3 (drop x (r!!y2))) (take 3 (drop x (r!!y3)))
        where
            y1 = y
            y2 = y + 1
            y3 = y + 2

-- returns true if all blocks in a sudoku are OK. (No digits twice in a row,
-- column, or square.
isOkay :: Sudoku -> Bool
isOkay s = isOkay' (blocks s)

isOkay' :: [Block] -> Bool
isOkay' []     = True
isOkay' (b:bs) = isOkayBlock b && isOkay' bs

-------------------------------------------------------------------------

type Pos = (Int, Int)

-- Returns the position of a blank element in a Sudoku.
blank :: Sudoku -> Pos
blank s = blank' (rows s) 0 0

blank' :: [[Maybe Int]] -> Int -> Int -> Pos
blank' s x 9 = error "blank': Sudoku is solved."
blank' s 9 y = blank' s 0 (y+1)
blank' s x y
    | (s!!y)!!x == Nothing = (x, y)
    | otherwise            = blank' s (x+1) y

prop_blank :: Sudoku -> Bool
prop_blank s@(Sudoku r) = (r!!y)!!x == Nothing
    where
        b = blank s
        x = fst b
        y = snd b

-- Replaces one element in a list. 
-- [1,    2, 3] !!= (0, 1337)
-- [1337, 2, 3]
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) a b = replace a [] b

-- Help function for (!!=)
replace :: [a] -> [a] -> (Int, a) -> [a]
replace (a:ar) b (0, x) = (reverse b) ++ [x] ++ ar
replace (a:ar) b (n, x) = replace ar (a:b) ((n-1), x)

prop_replace :: Ord a => [a] -> (Int, a) -> Property
prop_replace a (n, x) = 
    n >= 0 && n < length a ==>
        (a !!= (n, x))!!n == x

-- Updates a Sudoku with a new Maybe Int in a specific Pos.
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update s (x, y) i = Sudoku (update' r x y i)
    where
        r = rows s

update' :: [[Maybe Int]] -> Int -> Int -> Maybe Int -> [[Maybe Int]]
update' []     _ _ _ = []
update' (r:rs) x 0 i = r !!= (x, i) : update' rs x (-1) i
update' (r:rs) x y i = r            : update' rs x (y-1) i

prop_update :: Sudoku -> Pos -> Maybe Int -> Property
prop_update s p@(x, y) i = 
    0 <= x && x <= 8 && 0 <= y && y <= 8 ==>
        ((rows (update s p i))!!y)!!x == i

-------------------------------------------------------------------------

-- returns Just (solved sudoku) or Nothing.
solve :: Sudoku -> Maybe Sudoku
solve s | isOkay s == False = Nothing
        | isSolved s        = Just s
        | otherwise         = pick sud
	where
		bp = blank s
		sud = [solve (update s bp (Just n)) | n <- [1..9]]

-- Help function for solve.
-- picks the first Just a element in a [Maybe a]
pick :: [Maybe a] -> Maybe a
pick []           = Nothing
pick (Nothing:xs) = pick xs
pick (x:xs)       = x

-- Given a FilePath, print it's solution.
readAndSolve :: FilePath -> IO ()
readAndSolve path = do sud <- readSudoku path
                       printSudoku (fromJust (solve sud))

-- Returns true if a sudoku is a solution of another sudoku.
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf new old = isOkay new && isSolved new && isOf r1 r2
    where
        r1 = rows new
        r2 = rows old

-- Help function for isSolutionOf. It makes sure all the elements in the old
-- Sudoku are in the same place in the new solved Sudoku.
isOf :: [[Maybe Int]] -> [[Maybe Int]] -> Bool
isOf []           []                 = True
isOf ([]:xss)     ([]:yss)           = isOf xss yss
isOf ((x:xs):xss) ((Nothing:ys):yss) = isOf (xs:xss) (ys:yss)
isOf ((x:xs):xss) ((y:ys):yss) | x == y    = isOf (xs:xss) (ys:yss)
                               | otherwise = False

prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = isOkay s ==> isSolutionOf (fromJust (solve s)) s
