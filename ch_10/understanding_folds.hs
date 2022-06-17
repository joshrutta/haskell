module UnderstandingFolds where

{-

--- 
Definitions of foldr and foldl posted here for reference

foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr f z xs =
    case xs of
        [] -> z
        (x:xs) -> f x (foldr f z xs)

foldl :: (b -> a -> b) -> b -> [a] -> b 
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

---

1. foldr (*) 1 [1..5] == foldl (flip (*)) 1 [1..5] == foldl (*) 1 [1..5]
b and c 

2. foldl (flip (*)) 1 [1..3]
 = foldl (flip (*)) ((flip (*)) 1 1) [2,3]
 = foldl (flip (*)) (flip (*) (1) 2) [3]
 = foldl (flip (*)) (flip (*) (1 * 2) 3) []
 = 6

 3. c

 4. a

 5  a. needs another arg - foldr (++) [] ["woot", "WOOT", "woot"]
    
    b. comparing [] and char - needs to be same type - foldr max ' ' "fear is the little death"
    
    c. and operates on Foldable Bool - need (&&) instead - foldr (&&) True [False, True]
    
    d. Will always return true - (||) with one arg as True will always return True
    
    e. show only takes one arg and converts to String. need to get rid of it and just
    use (++). foldl (++) "" (map show [1..5])
    
    f. foldr const 'a' [1..5] X Issue is const
      foldl wants an (a -> b -> b). You give it an (a -> b -> a)
      compiler deduces a must equal b
      Get message "No instance for (Num Char) arising from the literal ‘1’"

      Can fix by using same type for both args
      foldr const 'a' ['a'..'e'] or foldr const 1 [1..5]
    
    g. Same as f, can fix by foldr const '0' "tacos" (remember String = [Char])
    
    h. Similar to f but for foldl. 
    foldl wants an (a -> b -> a). You give it an (a -> b -> b)
    compiler deduces a must equal b

    Can fix by using same type for both args

    i. Same as h.

-} 