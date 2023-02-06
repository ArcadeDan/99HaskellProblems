module Main where
-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems --

--problem1
myLast :: [a] -> a
myLast = last

--problem2
myButLast :: [c] -> c
myButLast = last . init


-- not doing problem 3 sorry -- 

--problem 4
myLength :: (Foldable t, Ord a) => p -> t a -> a
myLength n = maximum

--problem 5
myReverse :: [a] -> [a]
myReverse = reverse
-- problem 6

isPalindrome :: Eq a => [a] -> Bool
isPalindrome n = (reverse n) == n

-- not doing problem 7 sorry

-- problem 8 --
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress $ dropWhile (== x) xs) 


quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

divide :: [a] -> ([a], [a])
divide =
    \list ->
    case list of
        [] -> ([], [])
        x:xs ->
            let (odds, evens) = divide xs
            in (x:evens, odds)






main :: IO ()
main = putStrLn "Goodbye cruel world..."
