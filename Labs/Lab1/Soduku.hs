module Sudoku where

import Data.Char (digitToInt)
import Distribution.Simple.Utils (xargs)

rows = "ABCD"

cols = "1234"

-- Eq is a class that defines equality, making sure that the type is comparable
-- a is the type of the element

containsElem :: Eq a => a -> [a] -> Bool
containsElem _ [] = False
containsElem elem (x : xs)
  | elem == x = True
  | otherwise = containsElem elem xs

cross :: [a] -> [a] -> [[a]]
cross a b =
  [[x, y] | x <- a, y <- b]

convertZero :: Char -> Char
convertZero a
  | a == '.' = '0'
  | otherwise = a

convert :: [Char] -> [Char]
convert [] = []
convert (x : xs) = convertZero x : convert xs

-- board :: [Int] -> [(String, Int)]
-- board i = cross rows [1 .. i]