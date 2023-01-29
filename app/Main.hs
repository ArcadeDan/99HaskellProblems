module Main where
-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems --

--problem1
myLast :: [a] -> a
myLast = last

--problem2
myButLast :: [c] -> c
myButLast = last . init
--problem3

--problem 4
myLength :: (Foldable t, Ord a) => p -> t a -> a
myLength n = maximum

--problem 5
myReverse :: [a] -> [a]
myReverse = reverse
-- problem 6

isPalindrome :: Eq a => [a] -> Bool
isPalindrome n = (reverse n) == n


--problem 7


main :: IO ()
main = putStrLn "Goodbye cruel world..."