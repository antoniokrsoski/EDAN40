-- Antonio Krsoski & Willard Rådborg

type AlignmentType = (String, String)

similarityScore :: [Char] -> [Char] -> Int
similarityScore xs [] = scoreSpace * length xs
similarityScore [] ys = scoreSpace * length ys
similarityScore (x : xs) (y : ys) =
  maximum
    [ score (x, y) + (similarityScore xs ys),
      (score (x, '-') + similarityScore xs (y : ys)),
      (score ('-', y) + similarityScore (x : xs) ys)
    ]

scoreMatch = 0

scoreMismatch = -1

scoreSpace = -1

string1 = "writers"

string2 = "vintner"

score (x, '-') = scoreSpace
score ('-', y) = scoreSpace
score (x, y)
  | x == y = scoreMatch
  | x /= y = scoreMismatch

attachHeads :: a -> a -> [([a], [a])] -> [([a], [a])]
attachHeads h1 h2 aList = [(h1 : xs, h2 : ys) | (xs, ys) <- aList]

-- attaches h1 to first list head and h2 to second list head,
-- aList is a list of list tuples.

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = filter (\x -> valueFcn x == maxValue) xs
  where
    maxValue = maximum (map valueFcn xs)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([], [])]
optAlignments (x : xs) [] = attachHeads x '-' (optAlignments xs [])
optAlignments [] (y : ys) = attachHeads '-' y (optAlignments [] ys)
optAlignments (x : xs) (y : ys) = maximaBy sim alignments
  where
    sim (a, b) = similarityScore a b
    alignments =
      concat
        [ attachHeads x y (optAlignments xs ys),
          attachHeads x '-' (optAlignments xs (y : ys)),
          attachHeads '-' y (optAlignments (x : xs) ys)
        ]


outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
    putStrLn("There are " ++ show (length (optAlignments string1 string2)) ++ " optimal alignments: \n") 
    putStrLn("These are the optimal alignments " ++ show (optAlignments string1 string2))


mcsLength :: Eq a => [a] -> [a] -> Int
mcsLength xs ys = mcsLen (length xs) (length ys)
  where
    mcsLen i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]
       
    mcsEntry :: Int -> Int -> Int
    mcsEntry _ 0 = 0
    mcsEntry 0 _ = 0
    mcsEntry i j
      | x == y    = 1 + mcsLen (i-1) (j-1)
      | otherwise = max (mcsLen i (j-1)) 
                        (mcsLen (i-1) j)
      where
         x = xs!!(i-1)
         y = ys!!(j-1)



-- Case 1: Empty list of list tuples
-- Case 2: At least one element in list1 and empty list2 ->
--  attach x as head of first list and '-' as head of second list. The lists
--  being returned from recursive call of optAlignments on remaining list contents.
-- Case 3: Same as 2 except first is empty and second is not.
-- Case 4: Both lists have contents -> Create all alignments by concatenating
--  the result of attaching both heads to respective list, attaching x as head for first and '-' as head for second
--  and vice versa. Then perform maximaBy on these three different results to get the
--  best one.