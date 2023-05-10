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

-- Case 1: Empty list of list tuples
-- Case 2: At least one element in list1 and empty list2 ->
--  attach x as head of first list and '-' as head of second list. The lists
--  being returned from recursive call of optAlignments on remaining list contents.
-- Case 3: Same as 2 except first is empty and second is not.
-- Case 4: Both lists have contents -> Create all alignments by concatenating
--  the result of attaching both heads to respective list, attaching x as head for first and '-' as head for second
--  and vice versa. Then perform maximaBy on these three different results to get the
--  best one.

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
    putStrLn("There are " ++ show (length (optAlignmentsOptimization string1 string2)) ++ " optimal alignments: \n") 
    putStrLn("These are the optimal alignments " ++ show (optAlignmentsOptimization string1 string2))

similarityScoreOptimization :: String -> String -> Int
similarityScoreOptimization xs ys = simScore (length xs) (length ys)
    where
        simScore :: Int -> Int -> Int
        simScore i j = grid !! i !! j

        grid :: [[Int]]
        grid = [[entry i j | j <- [0..]] | i <- [0..]]

        entry :: Int -> Int -> Int
        entry 0 0 = 0
        entry i 0 = i * scoreSpace
        entry 0 j = j * scoreSpace
        entry i j = maximum [simScore (i - 1) (j - 1) + score (x, y),
                             simScore (i - 1)  j      + score (x, '-'),
                             simScore  i      (j - 1) + score ('-', y)]
            where
                x = xs !! (i - 1)
                y = ys !! (j - 1)

optAlignmentsOptimization :: String -> String -> [AlignmentType]
optAlignmentsOptimization xs ys = map
                          (pairApply reverse)
                          (snd $ alignment (length xs) (length ys))
    where
        pairApply f (a, b) = (f a, f b)

        alignment :: Int -> Int -> (Int, [AlignmentType])
        alignment i j = grid !! i !! j

        grid :: [[(Int, [AlignmentType])]]
        grid = [[entry i j | j <- [0..]] | i <- [0..]]

        entry :: Int -> Int -> (Int, [AlignmentType])
        entry 0 0 = (0, [([], [])])
        entry i 0 = (i * scoreSpace, [(take i xs, replicate i '-')])
        entry 0 j = (j * scoreSpace, [(replicate j '-', take j ys)])
        entry i j = (fst $ head opt, concat [snd b | b <- opt])
            where 
                (s1, a1) = alignment (i - 1) (j - 1)
                (s2, a2) = alignment (i - 1)  j
                (s3, a3) = alignment  i      (j - 1)
                x = xs !! (i - 1)
                y = ys !! (j - 1)
                opt = maximaBy fst $ [
                    (s1 + score (x,y),   attachHeads x y   a1),
                    (s2 + score (x, '-'), attachHeads x '-' a2),
                    (s3 + score ('-', y), attachHeads '-' y a3)]

