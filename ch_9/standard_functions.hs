module StandardFunctions where

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs) =
  if x == False
    then False
    else myAnd xs

myAnd2 :: [Bool] -> Bool
myAnd2 [] = True
myAnd2 (x : xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) =
  if x == True
    then True
    else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x : xs)
  | f x = True
  | otherwise = myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem y [] = False
myElem y (x : xs)
  | y == x = True
  | otherwise = myElem y xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 y = any (== y)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x : xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap (takeWhile (const True))

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : xs) = go f (x : xs) x
  where
    go f [] max = max
    go f (x : xs) max = if f x max == GT then go f xs x else go f xs max

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x : xs) = go f (x : xs) x
  where
    go f [] min = min
    go f (x : xs) min = if f x min == LT then go f xs x else go f xs min

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare