module Sudoku where

import Data.Char (digitToInt)
import Distribution.Simple.Utils (xargs)
import GHC.Float (int2Float)

rows = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

cols = "123456789"

-- Eq is a class that defines equality, making sure that the type is comparable
-- a is the type of the element

containsElem :: Eq a => a -> [a] -> Bool
containsElem _ [] = False
containsElem elem (x : xs)
  | elem == x = True
  | otherwise = containsElem elem xs

-- Task 0

cross :: [a] -> [a] -> [[a]]
cross a b =
  [[x, y] | x <- a, y <- b]

-- Task 1

convertZero :: Char -> Char
convertZero a
  | a == '.' = '0'
  | otherwise = a

convert :: [Char] -> [Char]
convert [] = []
convert (x : xs) = convertZero x : convert xs

-- Task 2
-- Only 4x4 and 9x9 since I don't know how to solve the cols list with more than values up to 9

board :: Int -> [String]
board n =
  let p = sqrt (int2Float n)
   in cross (take (round p) rows) (take (round p) cols)

-- Task 3

-- parseBoard :: [Char] -> [String]

-- parseBoard a = cross (board (length a)) (convert a)
