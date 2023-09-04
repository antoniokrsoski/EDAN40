import Data.List

fac n =
  if n <= 1
    then 1
    else n * fac (n - 1)

fac2 n
  | n <= 1 = 1
  | otherwise = n * fac2 (n - 1)

-- pattern matching with integers, not interesting
is_zero 0 = True
is_zero _ = False

-- tail recursion, what we really want, compute before recursion!!!!
-- we compute the new n and the new acc before calling the function again
-- this is good, no stackoverflow, no momeory habibi, everything good!
fac3 n = aux n 1
  where
    aux n acc
      | n <= 1 = acc
      | otherwise = aux (n - 1) (n * acc)

asc n m
  | m < n = []
  | m == n = [m]
  | m > n = n : asc (n + 1) m

sum1 [] = 0
sum1 (x : xs) = x + sum1 xs

elem1 :: (Eq a) => a -> [a] -> Bool
elem1 _ [] = False
elem1 a (x : xs)
  | a == x = True
  | otherwise = elem1 a xs

nub1 :: (Eq a) => [a] -> [a]
nub1 [] = []
nub1 (x : xs)
  | elem1 x xs == True = nub1 xs
  | otherwise = x : nub1 xs

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x : xs) =
  head xs == x + 1 && isAsc xs

data Tree a = Leaf | Node (Tree a) a (Tree a)

rev :: [a] -> [a]
rev = foldl (\acc x -> x : acc) []

prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x] : (map ((:) x) acc)) []

test :: (Monad m, Num b) => m b -> m b -> m b
test mx my = do
  x <- mx
  y <- my
  return $ x + y

g = (\x -> map ($ x))

oneOf :: Bool -> Bool -> Bool -> Bool
oneOf a b c
  | not (aorb) = c
  | not (borc) = a
  | not (aorc) = b
  | otherwise = False

oneOf2 _ _ c = c
oneOf2 a _ _ = a
oneOf2 _ b _ = b
oneOf2 _ _ _ = False