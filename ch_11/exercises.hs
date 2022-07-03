module Exercises where

import Data.Int

-- Pity The Bool

data BigSmall = Big Bool | Small Bool deriving (Eq, Show)

data NumberOrBool
  = Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

data QuantumBool
  = QuantumTrue
  | QuantumFalse
  | QuantumBoth
  deriving (Eq, Show)

data TwoQs = MkTwoQs QuantumBool QuantumBool

type TwoQuants = (QuantumBool, QuantumBool)

-- How does your Garden Grow?

data FlowerType
  = Gardenia
  | Daisy
  | Rose
  | Lilac
  deriving (Show)

type Gardener = String

data Garden = Garden Gardener FlowerType deriving (Show)

data GardenNormal
  = GardeniaNormal Gardener
  | DaisyNormal Gardener
  | RoseNormal Gardener
  | LilacNormal Gardener
  deriving (Show)

-- Programmers

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show, Enum)

data ProgLang = Haskell | Ada | Idris | PureScript deriving (Eq, Show, Enum)

data Programmer = Programmer
  { os :: OperatingSystem,
    lang :: ProgLang
  }
  deriving (Eq, Show)

allProgrammers :: [Programmer]
allProgrammers =
  [ Programmer os lang
    | os <- enumFrom GnuPlusLinux,
      lang <- enumFrom Haskell
  ]

-- Exponentiation in what order?

data Quantum = Yes | No | Both deriving (Eq, Show)

convert1 :: Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes = True
convert2 No = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes = True
convert3 No = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes = False
convert4 No = True
convert4 Both = True

convert5 :: Quantum -> Bool
convert5 Yes = True
convert5 No = False
convert5 Both = False

convert6 :: Quantum -> Bool
convert6 Yes = False
convert6 No = False
convert6 Both = True

convert7 :: Quantum -> Bool
convert7 Yes = False
convert7 No = True
convert7 Both = False

convert8 :: Quantum -> Bool
convert8 Yes = False
convert8 No = False
convert8 Both = False

-- The Quad

data Quad = One | Two | Three | Four deriving (Eq, Show)

{-

1. eQuad :: Either Quad Quad
Either is a sum type, so |eQuad| = 4 + 4 = 8

2. prodQuad :: (Quad, Quad)
|prodQuad| = 4 * 4 = 16

3. funcQuad :: Quad -> Quad
|funcQuad| = 4^4 = 256

4. prodTBool :: (Bool, Bool, Bool)
|prodTBool| = 2 * 2 * 2 = 8

5. gTwo :: Bool -> Bool -> Bool
|gTwo| = 2^2^2 = 16

6. fTwo :: Bool -> Quad -> Quad
|fTwo| = (4^4)^2 = 256^2 = 65536

-}

-- Binary Tree exercises

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
  if mapTree (+ 1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

-- Convert Binary Trees to Lists

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree =
  Node
    (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears."

-- foldr for BinaryTree

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b bintree = foldr f b (preorder bintree)