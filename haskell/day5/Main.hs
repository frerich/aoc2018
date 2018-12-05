import Data.Char (toUpper)


type Unit = Char
type Polymer = [Unit]


main :: IO ()
main = do
    polymer <- readFile "input.txt"
    print (length polymer)
    putStrLn ("Part one: " ++ show (partOne polymer))


partOne :: Polymer -> Int
partOne = length . converge react


canReact :: Unit -> Unit -> Bool
canReact a b = toUpper a == toUpper b && a /= b


react :: Polymer -> Polymer
react (a:b:rest)
    | react a b = react rest
    | otherwise = a : react (b:rest)
react xs         = xs


-- | Iteratively applies a function until it converges to some value
converge :: Eq a => (a -> a) -> a -> a
converge f xs = let xss = iterate f xs
                in fst (head (dropWhile (uncurry (/=)) (zip xs (tail xs))))
