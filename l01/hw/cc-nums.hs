-- Excercise 1
--
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = if n > 0 then toDigits (n `div` 10 ) ++ [n `mod` 10]
                      else []


toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = if n > 0 then [n `mod` 10] ++ toDigitsRev (n `div` 10 )
                         else []


-- Excercise 2
--
-- (|||) operator stolen from
-- https://mail.haskell.org/pipermail/libraries/2008-August/010496.html
(|||) :: [a] -> [a] -> [a]
[]     ||| ys = ys
(x:xs) ||| ys = x : ys ||| xs
infixr 5 |||

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []  = []
doubleEveryOther [n] = [n]
doubleEveryOther n   =
    reverse $ xs ||| (map (2*) ys)
        where (xs, ys) = splitList $ reverse n


doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' xs
    | len `mod` 2 == 0  = zipWith (*) xs $ cycle [2,1]
    | len `mod` 2 == 1  = zipWith (*) xs $ cycle [1,2]
    where len = length xs


splitList []  = ([], [])
splitList [x] = ([x], [])
splitList (x:y:zs) = (x:xs, y:ys)
    where (xs, ys) = splitList zs


-- Excercise 3
--
sumDigits :: [Integer] -> Integer
sumDigits n = sum $ concatMap toDigits n


-- Excercise 4
--
validate :: Integer -> Bool
validate n =
    sumDigits (doubleEveryOther (toDigits (n))) `mod` 10 == 0
