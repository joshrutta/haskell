module ScansExercises where

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN x = fibs !! x


-- 1

first20Fibs :: [Integer]
first20Fibs = take 20 fibs

-- 2

lessThan100Fibs :: [Integer]
lessThan100Fibs = [x | x <- fibs, x < 100]

-- 3

-- was trying to think of reecursive sol 
-- similar to fibs, but couldn't think of one :(
factorial :: [Integer]
factorial = scanl (*) 1 [2..]