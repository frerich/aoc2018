import Data.Char (isDigit)
import Data.List (sortOn, groupBy)
import Data.Function (on)


type Coord = (Int,Int)
type Area = [Coord]


boundingRect :: [Coord] -> (Coord,Coord)
boundingRect coords = let (xs,ys) = (map fst coords, map snd coords) in ((minimum xs, minimum ys), (maximum xs, maximum ys))


main :: IO ()
main = do
    coords <- parse <$> readFile "input.txt"
    putStrLn ("Part one: " ++ show (partOne coords))
    putStrLn ("Part two: " ++ show (partTwo coords))


partOne :: [Coord] -> Int
partOne coords = maximum . map length . filter (not . isInfinite) $ areas
 where
   ((minX,minY),(maxX,maxY)) = boundingRect coords
   allCoords = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]
   closestCoords = [(c,p) | c <- allCoords, Just p <- [closest coords c]]
   areas = map (map fst) . groupBy ((==) `on` snd) . sortOn snd $ closestCoords
   isInfinite = any (\(x,y) -> x == minX || x == maxX || y == minY || y == maxY)


partTwo :: [Coord] -> Int
partTwo coords = length (filter isSafe allCoords)
  where
   ((minX,minY),(maxX,maxY)) = boundingRect coords
   allCoords = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]
   isSafe pos = sum (map (manhattanDistance pos) coords) < 10000


closest :: [Coord] -> Coord -> Maybe Coord
closest coords pos = case sortOn (manhattanDistance pos) coords of
    (a:b:_) | manhattanDistance pos a < manhattanDistance pos b -> Just a
    _                                                           -> Nothing


manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x0,y0) (x1,y1) = abs (x0-x1) + abs (y0-y1)


parse :: String -> [Coord]
parse = map parseLine . lines
  where
    parseLine s = (read x, read y)
      where
        (x, rest) = span isDigit s
        (_, y) = break isDigit rest

