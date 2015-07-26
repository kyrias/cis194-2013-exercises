module List where

import Data.List

data List a = Cons a (List a)
            | Nil
            deriving (Show)



fromList (Cons x xs) = x : (fromList xs)
fromList Nil         = []

myLength :: Integral a => [a] -> Int
myLength []     = 0
myLength (x:xs) = (+) 1 $ myLength xs

mySum :: Integral a => [a] -> a
mySum []     = 0
mySum (x:xs) = (+) x $ mySum xs

mean :: (Integral a, Fractional b) => [a] -> b
mean xs = fromIntegral (mySum xs) / fromIntegral (myLength xs)

palindrome :: [a] -> [a]
palindrome xs = xs ++ reverse xs

mySort xs = sortBy (\x y -> compare (length x) (length y)) xs


myIntersperse :: a -> [[a]] -> [a]
myIntersperse _   [] = []
myIntersperse sep (x:xs)
    | null xs   = x
    | otherwise = x ++ sep : myIntersperse sep xs
