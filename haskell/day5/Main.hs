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
partOne = length . converge react


canReact :: Unit -> Unit -> Bool
canReact a b = toUpper a == toUpper b && a /= b


react :: Polymer -> Polymer
react (a:b:rest)
    | canReact a b = react rest
    | otherwise    = a : react (b : rest)
react xs           = xs


-- | Iteratively applies a function until it converges to some value
converge :: Eq a => (a -> a) -> a -> a
converge f xs = let xss = iterate f xs
                in fst (head (dropWhile (uncurry (/=)) (zip xss (tail xss))))


partTwo :: Polymer -> Int
partTwo polymer = minimum (map (length . converge react) candidates)
  where
    candidates = map (\u -> stripUnit u polymer) (nub (map toUpper polymer))


stripUnit :: Unit -> Polymer -> Polymer
stripUnit u = filter (`notElem` [toLower u, toUpper u])
