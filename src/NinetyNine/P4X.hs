-- 46 - 50
-- https://wiki.haskell.org/99_questions/46_to_50

module NinetyNine.P4X where

import Control.Arrow ((&&&), (***), first, second)
import Data.Function (on)
import Data.List (insertBy, sortOn)

{-
46. Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2
(for logical equivalence) which succeed or fail according to the result
of their respective operations;
e.g. and(A,B) will succeed, if and only if both A and B succeed.

A logical expression in two variables can then be written
as in the following example: and(or(A,B),nand(A,B)).

Now, write a predicate table/3 which prints the truth table of a given
logical expression in two variables.

Example:
(table A B (and A (or A B)))
true true true
true fail true
fail true fail
fail fail fail

Example in Haskell:
> table (\a b -> (and' a (or' a b)))
True True True
True False True
False True False
False False False
-}

equ' :: Bool -> Bool -> Bool
equ' = (==)

and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

nand' :: Bool -> Bool -> Bool
nand' x y = not $ and' x y

nor' :: Bool -> Bool -> Bool
nor' x y = not $ or' x y

xor' :: Bool -> Bool -> Bool
xor' x y = not $ equ' x y

impl' :: Bool -> Bool -> Bool
impl' x y = or' y $ not x

table :: (Bool -> Bool -> Bool) -> [[Bool]]
table f = [[x, y, f x y] | x <- [True, False], y <- [True, False]]

{-
47. Truth tables for logical expressions (2).
Continue problem P46 by defining and/2, or/2, etc as being operators.
This allows to write the logical expression in the more natural way,
as in the example: A and (A or not B). Define operator precedence as usual;
i.e. as in Java.

Example:
* (table A B (A and (A or not B)))
true true true
true fail true
fail true fail
fail fail fail

Example in Haskell:
> table2 (\a b -> a `and'` (a `or'` not b))
True True True
True False True
False True False
False False False
-}

infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'`

{-
48. Truth tables for logical expressions (3).
Generalize problem P47 in such a way that the logical expression may contain
any number of logical variables. Define table/2 in a way that table(List,Expr)
prints the truth table for the expression Expr, which contains the logical
variables enumerated in List.

Example:
* (table (A,B,C) (A and (B or C) equ A and B or A and C))
true true true true
true true fail true
true fail true true
true fail fail true
fail true true true
fail true fail true
fail fail true true
fail fail fail true

Example in Haskell:
> tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)

-- infixl 3 `equ'`
True  True  True  True
True  True  False True
True  False True  True
True  False False True
False True  True  True
False True  False True
False False True  True
False False False True

-- infixl 7 `equ'`
True  True  True  True
True  True  False True
True  False True  True
True  False False False
False True  True  False
False True  False False
False False True  False
False False False False
-}

genBools :: Integral n => n -> [[Bool]]
genBools = map reverse . h
  where
    h 0 = [[]]
    h n = f =<< h (pred n)
    f xs = ($ xs) <$> [(True :), (False :)]

tablen :: Integral n => n -> ([Bool] -> Bool) -> [[Bool]]
tablen n f = (\xs -> xs ++ [f xs]) <$> genBools n

{-
49. Gray codes.
An n-bit Gray code is a sequence of n-bit strings constructed according to
certain rules.
For example,
n = 1: C(1) = ['0','1'].
n = 2: C(2) = ['00','01','11','10'].
n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
Find out the construction rules and write a predicate with the following
specification:
% gray(N,C) :- C is the N-bit Gray code
Can you apply the method of "result caching" in order to make the predicate
more efficient, when it is to be used repeatedly?

Example in Haskell:
P49> gray 3
["000","001","011","010","110","111","101","100"]
-}

gray :: Integral n => n -> [String]
gray 0 = [""]
gray x =
  uncurry (++) . (map ('0' :) *** map ('1' :)) .
  (id &&& reverse) . gray . subtract 1 $ x

{-
50. Huffman codes.
We suppose a set of symbols with their frequencies, given as a list of fr(S,F)
terms. Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)].
Our objective is to construct a list hc(S,C) terms, where C is the Huffman
code word for the symbol S.
In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'),
hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.].
The task shall be performed by the predicate huffman/2 defined as follows:
% huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs

Example in Haskell:
*Exercises> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
[('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
-}

data HTree a = HLeaf a | HBranch (HTree a) (HTree a)
  deriving (Eq, Read, Show)

huffman :: (Ord a, Ord n, Num n) => [(a, n)] -> [(a, String)]
huffman = sortOn fst . serialize . htree . leaves
  where
    leaves :: (Ord n, Num n) => [(a, n)] -> [(HTree a, n)]
    leaves = sortOn snd . map (first HLeaf)

    htree :: (Ord n, Num n) => [(HTree a, n)] -> HTree a
    htree [(t, _)] = t
    htree (x1 : x2 : xs) = htree . insertOn snd (merge x1 x2) $ xs

    merge :: Num n => (HTree a, n) -> (HTree a, n) -> (HTree a, n)
    merge (t1, w1) (t2, w2) = (HBranch t1 t2, w1 + w2)

    insertOn :: Ord b => (a -> b) -> a -> [a] -> [a]
    insertOn = insertBy . (compare `on`)

    serialize :: HTree a -> [(a, String)]
    serialize (HLeaf x) = [(x, [])]
    serialize (HBranch l r) = f =<< [(l, '0'), (r, '1')]
      where
        f (t, b) = second (b :) <$> serialize t
