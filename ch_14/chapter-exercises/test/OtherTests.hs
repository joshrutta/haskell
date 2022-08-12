module OtherTests where

import Data.Char (toUpper)
import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

--  From ch 11 exercises

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x : xs)
  | x /= ' ' = toUpper x : xs
  | otherwise = x : capitalizeWord xs

-- 1

half :: Float -> Float
half x = x / 2

halfIdentity :: Float -> Bool
halfIdentity x = ((* 2) . half $ x) == x

-- 2

listOrdered :: Ord a => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

prop_listOrderedFloat :: [Float] -> Bool
prop_listOrderedFloat = listOrdered . sort

prop_listOrderedChar :: [Char] -> Bool
prop_listOrderedChar = listOrdered . sort

-- 3

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y =
  x + y == y + x

-- 4

multAssociative :: Rational -> Rational -> Rational -> Bool
multAssociative x y z =
  x * (y * z) == (x * y) * z

multCommutative :: Rational -> Rational -> Bool
multCommutative x y =
  x * y == y * x

-- 5

prop_quot :: Int -> Int -> Bool
prop_quot x y = (quot x y) * y + (rem x y) == x

prop_div :: Int -> Int -> Bool
prop_div x y = (div x y) * y + (mod x y) == x

-- 6

-- subtraction is not associative
subAssociative :: Int -> Int -> Int -> Bool
subAssociative x y z =
  (x - y) - z == x - (y - z)

-- subtraction is not commutative
subCommutative :: Int -> Int -> Bool
subCommutative x y =
  x - y == y - x

-- exp is not associative
expAssociative :: Int -> Int -> Int -> Bool
expAssociative x y z =
  (x ^ y) ^ z == x ^ (y ^ z)

-- exp is not commutative
expCommutative :: Int -> Int -> Bool
expCommutative x y =
  x ^ y == y ^ x

-- 7

prop_reverseChar :: [Char] -> Bool
prop_reverseChar xs = (reverse . reverse) xs == (id xs)

-- 8
{-
annoying bc QuickCheck requires arguments have instance of Show
which functions don't have. Luckily (!) this is common enough I
suppose that Quickcheck has some built in stuff to handle functions
https://hackage.haskell.org/package/QuickCheck-2.13.1/docs/Test-QuickCheck.html#g:14
Still don't understand atm, hopefully Functor chapter will clear this up
-}

prop_applicationCharInt :: Fun Char Int -> Char -> Bool
prop_applicationCharInt (Fun _ f) x = (f $ x) == (f x)

prop_compositionCharIntAndIntCharFunctions :: Fun Char Int -> Fun Int Char -> Int -> Bool 
prop_compositionCharIntAndIntCharFunctions (Fun _ f) (Fun _ g) x = (f . g) x == f (g x)

-- 9

prop_concat1 :: [Int] -> Bool
prop_concat1 xs = foldr (:) [] xs == (++) [] xs 

prop_concat2 :: [String] -> Bool
prop_concat2 xs = foldr (++) [] xs == concat xs 

-- 10

-- Fails bc sometimes list may not be long enough

prop_take_length :: Int -> [Char] -> Bool
prop_take_length n xs = length (take n xs) == n

-- 11

prop_read_show :: String -> Bool
prop_read_show x = (read $ show x) == x  

-- Failure
{-

square x = x * x 

squareIdentity = square . sqrt 

Fails bc sqrt of negative numbers is NaN

-}

-- Idempotence 

twice f = f . f 
fourTimes = twice . twice 

-- 1
f :: String -> Bool 
f x = (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == fourTimes capitalizeWord x)

-- 2
f' :: [Int] -> Bool
f' x = (sort x == twice sort x) && (sort x == fourTimes sort x)

main :: IO ()
main = hspec $ do
  describe "half" $ do
    it "(x * 2) / 2 is equal to x" $ property halfIdentity

  describe "sort" $ do
    it "prop_listOrderedFloat holds true" $ property prop_listOrderedFloat
    it "prop_listOrderedChar holds true" $ property prop_listOrderedChar

  describe "addition properties" $ do
    it "addition is associative" $ property plusAssociative
    it "addition is commutative" $ property plusCommutative

  describe "multiplication properties" $ do
    it "multiplication is associative" $ property multAssociative
    it "multiplication is commutative" $ property multCommutative

  describe "quot and div properties" $ do
    it "quot property " $ property multAssociative
    it "div property" $ property multCommutative

  --   describe "subtraction properties" $ do
  --     it "subtraction is associative" $ property subAssociative
  --     it "subtraction is commutative" $ property subCommutative

  --   describe "exponentiation properties" $ do
  --     it "exponentiation is associative" $ property expAssociative
  --     it "exponentiation is commutative" $ property expCommutative

  describe "reverse  property" $ do
    it "reverse property" $ property prop_reverseChar

  describe "application operator property" $ do
    it "application operator property" $ property prop_applicationCharInt

  describe "composition operator property" $ do 
    it "(f . g) x == f (g x)" $ property prop_compositionCharIntAndIntCharFunctions

  describe "concat operator properties" $ do 
    it "foldr (:) == (++)" $ property prop_concat1
    it "foldr (++) [] == concat" $ property prop_concat2
  
  -- describe "length take property" $ do 
  --   it "length (take n xs) == n" $ property prop_take_length

  describe "read-show property" $ do 
    it "read $ show x == x" $ property prop_read_show

  
  describe "idempotent function tests" $ do 
    it "capitalizeWord test" $ property f 
    it "sort test" $ property f'