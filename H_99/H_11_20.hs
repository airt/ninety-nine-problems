
--Problem 11-20
--https://wiki.haskell.org/99_questions/11_to_20

module H_99.H_11_20
( Elem(..)
, encodeModified
, decodeModified
, encodeDirect
, dupli
, repli
, dropEvery
, split
, slice
, rotate
, removeAt
) where

import H_99.H_01_10
import Data.List

{-
11. Modified run-length encoding.
Modify the result of problem 10 in such a way that if an element has no
duplicates it is simply copied into the result list. Only elements with
duplicates are transferred as (N E) lists.

Example:
* (encode-modified '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))

Example in Haskell:
P11> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

data Elem a = Single a | Multiple Int a
  deriving (Eq, Show, Read)

encodeModified :: (Eq a) => [a] -> [Elem a]
encodeModified xs = map f . encode $ xs
  where
    f (1, x) = Single x
    f (n, x) = Multiple n x

{-
12. Decode a run-length encoded list.
Given a run-length code list generated as specified in problem 11.
Construct its uncompressed version.

Example in Haskell:
P12> decodeModified
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"
-}

decodeModified :: (Eq a) => [Elem a] -> [a]
decodeModified [] = []
decodeModified ((Single y):xs) = [y] ++ decodeModified xs
decodeModified ((Multiple n y):xs) = genericReplicate n y ++ decodeModified xs

{-
13. Run-length encoding of a list (direct solution).
Implement the so-called run-length encoding data compression method directly.
I.e. don't explicitly create the sublists containing the duplicates,
as in problem 9, but only count them. As in problem P11,
simplify the result list by replacing the singleton lists (1 X) by X.

Example:
* (encode-direct '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))

Example in Haskell:
P13> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

encodeDirect :: (Eq a) => [a] -> [Elem a]
encodeDirect xs = foldr f [] xs
  where
    f x [] = [Single x]
    f x ((Single y):zs)
      | x == y = Multiple 2 x : zs
    f x ((Multiple n y):zs)
      | x == y = Multiple (n + 1) x : zs
    f x acc = Single x : acc

{-
14. Duplicate the elements of a list.

Example:
* (dupli '(a b c c d))
(A A B B C C C C D D)

Example in Haskell:
> dupli [1, 2, 3]
[1,1,2,2,3,3]
-}

dupli :: [a] -> [a]
dupli xs = repli xs 2

{-
15. Replicate the elements of a list a given number of times.

Example:
* (repli '(a b c) 3)
(A A A B B B C C C)

Example in Haskell:
> repli "abc" 3
"aaabbbccc"
-}

repli :: (Integral b) => [a] -> b -> [a]
repli xs n = concatMap (genericReplicate n) xs

{-
16. Drop every N'th element from a list.

Example:
* (drop '(a b c d e f g h i k) 3)
(A B D E G H K)

Example in Haskell:
*Main> dropEvery "abcdefghik" 3
"abdeghk"
-}

dropEvery :: (Integral b) => [a] -> b -> [a]
dropEvery xs n = map snd . filter ((/=0) . (`mod`n) . fst) . zip [1..] $ xs

{-
17. Split a list into two parts; the length of the first part is given.
Do not use any predefined predicates.

Example:
* (split '(a b c d e f g h i k) 3)
( (A B C) (D E F G H I K))

Example in Haskell:
*Main> split "abcdefghik" 3
("abc", "defghik")
-}

split :: (Integral b) => [a] -> b -> ([a], [a])
split xs n = foldr f ([], []) . zip [1..] $ xs
  where
    f (i,x) (ys,zs)
      | i <= n    = (x : ys, zs)
      | otherwise = (ys, x : zs)

{-
18. Extract a slice from a list.
Given two indices, i and k, the slice is the list containing the elements
between the i'th and k'th element of the original list (both limits included).
Start counting the elements with 1.

Example:
* (slice '(a b c d e f g h i k) 3 7)
(C D E F G)

Example in Haskell:
*Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
"cdefg"
-}

slice :: (Integral b) => [a] -> b -> b -> [a]
slice xs i k = genericTake (k - i + 1) . genericDrop (i - 1) $ xs

{-
19. Rotate a list N places to the left.
Hint: Use the predefined functions length and (++).

Examples:
* (rotate '(a b c d e f g h) 3)
(D E F G H A B C)
* (rotate '(a b c d e f g h) -2)
(G H A B C D E F)

Examples in Haskell:
*Main> rotate ['a','b','c','d','e','f','g','h'] 3
"defghabc"
*Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
"ghabcdef"
-}

rotate :: (Integral b) => [a] -> b -> [a]
rotate xs n = b ++ a
  where
    (a, b) = split xs . mod n . fromIntegral . length $ xs

{-
20. Remove the K'th element from a list.

Example in Prolog:
?- remove_at(X,[a,b,c,d],2,R).
X = b
R = [a,c,d]

Example in Lisp:
* (remove-at '(a b c d) 2)
(A C D)
(Note that this only returns the residue list,
 while the Prolog version also returns the deleted element.)

Example in Haskell:
*Main> removeAt 2 "abcd"
('b',"acd")
-}

removeAt :: (Integral b) => b -> [a] -> (a, [a])
removeAt k xs = foldr f (head xs, []) . zip [1..] $ xs
  where
    f (i,x) (z,zs)
      | i == k    = (x, zs)
      | otherwise = (z, x : zs)
