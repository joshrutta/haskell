module ChapterExercises where

type Name = String

type Age = Integer

type ValidatePerson a = Either [PersonInvalid] a

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  deriving (Eq, Show)

data Person = Person Name Age deriving (Show)

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age =
  if age >= 0
    then Right age
    else Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name =
  if name /= ""
    then Right name
    else Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' ::
  ValidatePerson Name ->
  ValidatePerson Age ->
  ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge

-- Determine the kinds
{-
1. id:: a -> a
What is the kind of a?

:k a :: *

2. r :: a -> f a
What is the kinds of a and f?

:k a :: *
:k f :: * -> *

-}

-- String Processing

-- 1

notThe :: String -> Maybe String
notThe input = if input == "the" then Nothing else Just input

replaceNothingWithA :: Maybe String -> String
replaceNothingWithA (Just input) = input
replaceNothingWithA Nothing = "a"

replaceThe :: String -> String
replaceThe input = unwords $ map (replaceNothingWithA . notThe) $ words input

-- 2

isVowel :: Char -> Bool
isVowel c = c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'

countTheBeforeVowel' :: [String] -> Integer
countTheBeforeVowel' [] = 0
countTheBeforeVowel' [_] = 0
countTheBeforeVowel' (word1 : word2@(x : xs) : restOfWords)
  | word1 == "the" && isVowel x = 1 + countTheBeforeVowel' restOfWords
  | otherwise = countTheBeforeVowel' (word2 : restOfWords)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel input = countTheBeforeVowel' $ words input

-- 3

countVowels :: String -> Integer
countVowels sentence = toInteger $ length $ filter isVowel sentence

-- Validate the word

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord input =
  if countVowels input > (toInteger (length input) - countVowels input)
    then Nothing
    else Just $ Word' input

-- It's only Natural

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat z
  | z < 0 = Nothing
  | otherwise = Just $ integerToNat' z

integerToNat' :: Integer -> Nat
integerToNat' z
  | z == 0 = Zero
  | z > 0 = Succ $ integerToNat' (z -1)

-- Small library for Maybe

-- 1.

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing x = not $ isJust x

-- 2.

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee b f (Just a) = f a

-- 3.

fromMaybe :: a -> Maybe a -> a
fromMaybe b = mayybee b id

-- 4.

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5.

catMaybes :: [Maybe a] -> [a]
catMaybes maybeList = map (\(Just x) -> x) $ filter isJust maybeList

-- 6.

hasNothing :: [Maybe a] -> Bool
hasNothing [] = False
hasNothing (x : xs)
  | isNothing x = True
  | otherwise = hasNothing xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe x
  | hasNothing x = Nothing
  | otherwise = Just $ catMaybes x

-- Small library for Either

-- 1.
getLeft :: Either a b -> [a] -> [a]
getLeft (Left x) xs = x : xs
getLeft (Right _) xs = xs

lefts' :: [Either a b] -> [a]
lefts' = foldr getLeft []

-- 2.
getRight :: Either a b -> [b] -> [b]
getRight (Right x) xs = x : xs
getRight (Left _) xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr getRight []

-- 3.
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' f _ = Nothing

-- 5.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left a) = f a
either' f g (Right b) = g b

-- 6.
eitherMaybe2' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe2' g x = either' (const Nothing) (Just . g) x