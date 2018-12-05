import Data.Char (toLower, toUpper)
import Data.List (nub, minimum)


type Unit = Char
type Polymer = [Unit]


main :: IO ()
main = do
    polymer <- readFile "input.txt"
    putStrLn ("Part one: " ++ show (partOne polymer))
    putStrLn ("Part two: " ++ show (partTwo polymer))


partOne :: Polymer -> Int
partOne = length . react


canReact :: Unit -> Unit -> Bool
canReact a b = toUpper a == toUpper b && a /= b


react :: Polymer -> Polymer
react = foldr go ""
  where
    go x (y:ys) | canReact x y = ys
                | otherwise    = x : y : ys
    go x xs                    = x : xs


partTwo :: Polymer -> Int
partTwo polymer = minimum (map partOne candidates)
  where
    candidates = map (\u -> stripUnit u polymer) (nub (map toUpper polymer))


stripUnit :: Unit -> Polymer -> Polymer
stripUnit u = filter (`notElem` [toLower u, toUpper u])
