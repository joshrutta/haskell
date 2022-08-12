module OtherTests where

import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

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
