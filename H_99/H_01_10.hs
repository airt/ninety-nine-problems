
--Problem 1-10
--https://wiki.haskell.org/99_questions/1_to_10

module H_99.H_01_10
( NestedList(..)
, myLast
, myButLast
, elementAt
, myLength
, myReverse
, isPalindrome
, flatten
, compress
, pack
, encode
) where

{-
1. Find the last element of a list.
(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:
Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'
-}

myLast :: [a] -> a
myLast [] = error "myLast: empty list"
myLast xs = foldr1 (flip const) xs

myLast' = head . reverse

{-
2. Find the last but one element of a list.
(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:
Prelude> myButLast [1,2,3,4]
3
Prelude> myButLast ['a'..'z']
'y'
-}

myButLast :: [a] -> a
myButLast []     = error "myButLast: empty list"
myButLast [x]    = error "myButLast: one element list"
myButLast [x,_]  = x
myButLast (x:xs) = myButLast xs

myButLast' = (!!1) . reverse

{-
3. Find the K'th element of a list.
The first element in the list is number 1.

Example:
* (element-at '(a b c d e) 3)
c

Example in Haskell:
Prelude> elementAt [1,2,3] 2
2
Prelude> elementAt "haskell" 5
'e'
-}

elementAt :: [a] -> Int -> a
elementAt []     n = error "elementAt: index out of bounds"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n - 1)

{-
4. Find the number of elements of a list.

Example in Haskell:
Prelude> myLength [123, 456, 789]
3
Prelude> myLength "Hello, world!"
13
-}

myLength :: (Num b) => [a] -> b
myLength xs = foldr (const (+1)) 0 xs

myLength' = sum . map (const 1)

{-
5. Reverse a list.

Example in Haskell:
Prelude> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
Prelude> myReverse [1,2,3,4]
[4,3,2,1]
-}

myReverse :: [a] -> [a]
myReverse xs = foldl (flip (:)) [] xs

{-
6. Find out whether a list is a palindrome.
A palindrome can be read forward or backward;
e.g. (x a m a x).

Example in Haskell:
*Main> isPalindrome [1,2,3]
False
*Main> isPalindrome "madamimadam"
True
*Main> isPalindrome [1,2,4,8,16,8,4,2,1]
True
-}

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

{-
7. Flatten a nested list structure.
Transform a list, possibly holding lists as elements into a 'flat' list
 by replacing each list with its elements (recursively).

Example:
* (my-flatten '(a (b (c d) e)))
(A B C D E)

Example in Haskell:
We have to define a new data type, because lists in Haskell are homogeneous.
 data NestedList a = Elem a | List [NestedList a]
*Main> flatten (Elem 5)
[5]
*Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
*Main> flatten (List [])
[]
-}

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x)      = [x]
flatten (List [])     = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

{-
8. Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced
 with a single copy of the element.
The order of the elements should not be changed.

Example:
* (compress '(a a a a b c c a a d e e e e))
(A B C A D E)

Example in Haskell:
> compress "aaaabccaadeeee"
"abcade"
-}

compress :: (Eq a) => [a] -> [a]
compress xs = foldr f [] xs
  where
    f x [] = [x]
    f x z
      | x == head z = z
      | otherwise   = x : z

{-
9. Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements
 they should be placed in separate sublists.

Example:
* (pack '(a a a a b c c a a d e e e e))
((A A A A) (B) (C C) (A A) (D) (E E E E))

Example in Haskell:
*Main> pack "aaaabccaadeeee"
["aaaa","b","cc","aa","d","eeee"]
-}

pack :: (Eq a) => [a] -> [[a]]
pack xs = foldr f [] xs
  where
    f x [] = [[x]]
    f x acc@(z:zs)
      | x == head z = (x : z) : zs
      | otherwise   = [x] : acc

{-
10. Run-length encoding of a list.
Use the result of problem P09 to implement the so-called
 run-length encoding data compression method.
Consecutive duplicates of elements are encoded as lists (N E)
 where N is the number of duplicates of the element E.

Example:
* (encode '(a a a a b c c a a d e e e e))
((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

Example in Haskell:
encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}

encode :: (Eq a, Num b) => [a] -> [(b, a)]
encode xs = foldr f [] xs
  where
    f x [] = [(1, x)]
    f x acc@((n,y):zs)
      | x == y    = (1 + n, x) : zs
      | otherwise = (1, x) : acc
