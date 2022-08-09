module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise =
        go (n - d) d (count + 1)

multiplyBy :: (Eq a, Num a, Ord a) => a -> a -> a
multiplyBy a b = go a b 0
  where
    go a1 b1 acc
      | b1 == 0 = acc
      | b1 < 0 = go a1 (b1 + 1) (acc - a1)
      | b1 > 0 = go a1 (b1 - 1) (acc + a1)

main :: IO ()
main = hspec $ do
  describe "division and multiplication" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it
      "22 divided by 5 is\
      \ 4 remainder 2"
      $ do
        dividedBy 22 5 `shouldBe` (4, 2)
    it "3 times 5 is 15" $ do
      multiplyBy 3 5 `shouldBe` 15
    it "3 times -5 is -15" $ do
      multiplyBy 3 (-5) `shouldBe` (-15)
    it "-4 times 3 is -12" $ do
      multiplyBy (-4) 3 `shouldBe` (-12)
    it "-3 times -6 is 18" $ do
      multiplyBy (-3) (-6) `shouldBe` 18
    it
      "x + 1 is always\
      \ greater than x"
      $ do
        property $ \x -> (x :: Int) + 1 > x

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']