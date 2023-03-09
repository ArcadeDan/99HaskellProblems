module Main where
-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems --
import Data.List
import Control.Monad
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


calculate :: Integral a => a -> a -> a
calculate t b = sum $ filter (\x -> x `mod` 2 == 0) [b..t]   


divide :: [Int] -> ([Int], [Int])
divide =
    \list ->
    case list of
        [] -> ([], [])
        x:xs ->
            let (odds, evens) = divide xs
            in (x:evens, odds)



fibb :: Integer -> Integer
fibb =
    \numbers ->
    case numbers of
        0 -> 0
        1 -> 1
        n -> fibb(n-1) + fibb(n-2) 
        


-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack (x:xs) = let (first, rest) = span (==x) xs
                in (x:first) : pack rest
pack [] = []


-- Problem 10
encode :: Eq a => [a] -> [(Int, [a])]
encode (x:xs) = let (first, rest) = span (==x) xs
                    in (length (x:first), x:first) : encode rest
encode [] = []


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton' :: a -> Tree a
singleton' x = Node x EmptyTree EmptyTree

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton' x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

-- problem 11
data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "red gem"  
    show Green = "green gem"
    show Yellow = "yellow gem"    


main :: IO ()
main = do 
    rs <- sequence [getLine, getLine, getLine]
    print rs
        
