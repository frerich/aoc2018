import Data.List (group, sort, tails)


main :: IO ()
main = do
    ids <- lines <$> readFile "input.txt"
    putStrLn ("Part one: " ++ show (partOne ids))
    putStrLn ("Part two: " ++ show (partTwo ids))


partOne :: [String] -> Int
partOne ids = length idsWithDoublettes * length idsWithTriplets
  where
    idsWithDoublettes = filter (any ((== 2) . snd)) histograms
    idsWithTriplets = filter (any ((== 3) . snd)) histograms
    histograms = map histogram ids


partTwo :: [String] -> String
partTwo ids = [a | (a,b) <- zip idA idB, a == b]
  where
    (idA,idB) = head . filter (null . drop 1 . differences) . pairs $ ids
    differences (as,bs) = filter (uncurry (/=)) (zip as bs)


-- | Counts how often each element in the input list occurs
histogram :: Ord a => [a] -> [(a, Int)]
histogram = map (\g -> (head g, length g)) . group . sort


-- | Yields all ways to pick two elements from a list (ignoring ordering)
pairs :: [a] -> [(a,a)]
pairs xs = [(a,b) | (a:as) <- tails xs, b <- as]
