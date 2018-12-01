import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.List (scanl')


main :: IO ()
main = do
    frequency_changes <- parse <$> readFile "input.txt"
    putStrLn ("Part one: " ++ show (partOne frequency_changes))
    putStrLn ("Part two: " ++ show (partTwo frequency_changes))


parse :: String -> [Int]
parse = map parseInt . lines
  where
    parseInt ('+':xs) = read xs
    parseInt xs       = read xs


partOne :: [Int] -> Int
partOne = sum


partTwo :: [Int] -> Int
partTwo frequency_changes = head (dupes frequencies)
  where
    frequencies = scanl' (+) 0 (cycle frequency_changes)


dupes :: Ord a => [a] -> [a]
dupes = go Set.empty
  where
    go _    []                = []
    go seen (x:xs)
        | x `Set.member` seen = x : go (x `Set.insert` seen) xs
        | otherwise           = go (x `Set.insert` seen) xs

