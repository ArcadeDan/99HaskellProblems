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


main :: IO ()
main = putStrLn "Goodbye cruel world..."
