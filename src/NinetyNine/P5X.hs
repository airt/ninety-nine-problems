-- 54 - 60
-- https://wiki.haskell.org/99_questions/54A_to_60

module NinetyNine.P5X where

import Data.List (findIndex, genericIndex)
import Data.Maybe (fromJust)

data BTree a = Empty | Branch a (BTree a) (BTree a)
  deriving (Eq, Ord, Show)

{-
54A. Check whether a given term represents a binary tree.

In Prolog or Lisp, one writes a predicate to do this.
Example in Lisp:
* (istree (a (b nil nil) nil))
T
* (istree (a (b nil nil)))
NIL

Haskell's type system ensures that all terms of type Tree a are binary trees:
it is just not possible to construct an invalid tree with this type.
Hence, it is redundant to introduce a predicate to check this property:
it would always return True.
-}

{-
55. Construct completely balanced binary trees.

In a completely balanced binary tree, the following property holds for
every node: The number of nodes in its left subtree and the number of
nodes in its right subtree are almost equal, which means their difference
is not greater than one.

Write a function cbal-tree to construct completely balanced binary trees
for a given number of nodes. The predicate should generate all solutions
via backtracking. Put the letter 'x' as information into all nodes of
the tree.

Example:
* cbal-tree(4,T).
T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
etc......No

Example in Haskell,
whitespace and "comment diagrams" added for clarity and exposition:
*Main> cbalTrees 4
[ -- permutation 1
  --     x
  --    / \
  --   x   x
  --        \
  --         x
  Branch 'x' (Branch 'x' Empty Empty)
             (Branch 'x' Empty
                         (Branch 'x' Empty Empty))
, -- permutation 2
  --     x
  --    / \
  --   x   x
  --      /
  --     x
  Branch 'x' (Branch 'x' Empty Empty)
             (Branch 'x' (Branch 'x' Empty Empty)
                         Empty)
, -- permutation 3
  --     x
  --    / \
  --   x   x
  --    \
  --     x
  Branch 'x' (Branch 'x' Empty
                         (Branch 'x' Empty Empty))
             (Branch 'x' Empty Empty)
, -- permutation 4
  --     x
  --    / \
  --   x   x
  --  /
  -- x
  Branch 'x' (Branch 'x' (Branch 'x' Empty Empty)
                         Empty)
             (Branch 'x' Empty Empty)
]
-}

cbalTrees :: Integral n => a -> n -> [BTree a]
cbalTrees _ 0 = [Empty]
cbalTrees x n = f =<< [d .. d + m]
  where
    (d, m) = divMod (pred n) 2
    f i = Branch x <$> cbalTrees x i <*> cbalTrees x (pred n - i)

{-
56. Symmetric binary trees.

Let us call a binary tree symmetric if you can draw a vertical line through
the root node and then the right subtree is the mirror image of the left
subtree. Write a predicate symmetric/1 to check whether a given binary tree
is symmetric. Hint: Write a predicate mirror/2 first to check whether one
tree is the mirror image of another. We are only interested in the structure,
not in the contents of the nodes.

Example in Haskell:
*Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
False
*Main> symmetric (Branch 'x' (Branch 'x' Empty Empty)
                             (Branch 'x' Empty Empty))
True
-}

symmetric :: BTree a -> Bool
symmetric t = h t t
  where
    h Empty Empty = True
    h (Branch _ lx rx) (Branch _ ly ry) = h lx ry && h rx ly
    h _ _ = False

{-
57. Binary search trees (dictionaries).

Use the predicate add/3, developed in chapter 4 of the course,
to write a predicate to construct a binary search tree from a list of
integer numbers.

Example:
* construct([3,2,5,7,1],T).
T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))
Then use this predicate to test the solution of the problem P56.

Example:
* test-symmetric([5,3,18,1,4,12,21]).
Yes
* test-symmetric([3,2,5,7,4]).
No

Example in Haskell:
*Main> construct [3, 2, 5, 7, 1]
Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty)
                   (Branch 5 Empty (Branch 7 Empty Empty))
*Main> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
True
*Main> symmetric . construct $ [3, 2, 5, 7, 1]
True
-}

construct :: Ord a => [a] -> BTree a
construct = foldl (flip add) Empty
  where
    add x Empty = Branch x Empty Empty
    add x (Branch y l r) | x < y = Branch y (add x l) r
    add x (Branch y l r) | x > y = Branch y l (add x r)
    add _ t = t

{-
58. Generate-and-test paradigm.

Apply the generate-and-test paradigm to construct all symmetric,
completely balanced binary trees with a given number of nodes.

Example:
* sym-cbal-trees(5,Ts).
Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)),
      t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))]

Example in Haskell:
*Main> symCbalTrees 5
[ Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty))
             (Branch 'x' (Branch 'x' Empty Empty) Empty)
, Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty)
             (Branch 'x' Empty (Branch 'x' Empty Empty))
]
-}

symCbalTrees :: Integral n => a -> n -> [BTree a]
symCbalTrees x = filter symmetric . cbalTrees x

{-
59. Construct height-balanced binary trees.

In a height-balanced binary tree, the following property holds for every node:
The height of its left subtree and the height of its right subtree are almost
equal, which means their difference is not greater than one.

Construct a list of all height-balanced binary trees with the given element
and the given maximum height.

Example:
?- hbal_tree(3,T).
T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)),
         t(x, t(x, nil, nil), t(x, nil, nil))) ;
T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)),
         t(x, t(x, nil, nil), nil)) ;
etc......No

Example in Haskell:
*Main> take 4 $ hbalTreesH 'x' 3
[ Branch 'x' (Branch 'x' Empty Empty)
             (Branch 'x' Empty (Branch 'x' Empty Empty))
, Branch 'x' (Branch 'x' Empty Empty)
             (Branch 'x' (Branch 'x' Empty Empty) Empty)
, Branch 'x' (Branch 'x' Empty Empty)
             (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
, Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty))
             (Branch 'x' Empty Empty)
]
-}

hbalTreesH :: Integral n => a -> n -> [BTree a]
hbalTreesH _ 0 = [Empty]
hbalTreesH x 1 = [Branch x Empty Empty]
hbalTreesH x h = f =<< lrhs
  where
    lrhs = [(pred . pred, pred), (pred, pred), (pred, pred . pred)]
    f (lh, rh) = Branch x <$> hbalTreesH x (lh h) <*> hbalTreesH x (rh h)

hbalTreesH' :: Integral n => a -> n -> [BTree a]
hbalTreesH' x = genericIndex tss
  where
    tss = [Empty] : [Branch x Empty Empty] : zipWith h tss (tail tss)
    h xs ys = f =<< [(xs, ys), (ys, ys), (ys, xs)]
    f (ls, rs) = Branch x <$> ls <*> rs

{-
60. Construct height-balanced binary trees with a given number of nodes.

Consider a height-balanced binary tree of height H. What is the maximum
number of nodes it can contain?

Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN?
This question is more difficult.
Try to find a recursive statement and turn it into a function minNodes
that returns the minimum number of nodes in a height-balanced binary tree
of height H.
On the other hand, we might ask:
what is the maximum height H a height-balanced binary tree with N nodes
can have?
Write a function maxHeight that computes this.
Now, we can attack the main problem:
construct all the height-balanced binary trees with a given number of nodes.
Find out how many height-balanced trees exist for N = 15.

Example in Prolog:
?- count_hbal_trees(15,C).
C = 1553

Example in Haskell:
*Main> length $ hbalTrees 'x' 15
1553
*Main> map (hbalTrees 'x') [0..3]
[ [ Empty ]
, [ Branch 'x' Empty Empty ]
, [ Branch 'x' Empty (Branch 'x' Empty Empty)
  , Branch 'x' (Branch 'x' Empty Empty) Empty ]
, [ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty) ]
]
-}

maxNodesByHeight :: Integral n => n -> n
maxNodesByHeight = pred . (2 ^)

minNodesByHeight :: Integral n => n -> n
minNodesByHeight = genericIndex minNodesSequence

maxHeightByNodes :: Integral n => n -> n
maxHeightByNodes = fromIntegral . pred . fromJust . flip findIndex minNodesSequence . (<)

minHeightByNodes :: Integral n => n -> n
minHeightByNodes = ceiling . logBase 2 . fromIntegral . succ

minNodesSequence :: Integral n => [n]
minNodesSequence = fromIntegral <$> ns
  where
    ns = 0 : 1 : zipWith ((+) . succ) ns (tail ns) :: [Integer]

countNodes :: Integral n => BTree a -> n
countNodes Empty = 0
countNodes (Branch _ l r) = succ $ countNodes l + countNodes r

hbalTrees :: Integral n => a -> n -> [BTree a]
hbalTrees x n = [t | h <- [minHeightByNodes n .. maxHeightByNodes n], t <- hbalTreesH x h, countNodes t == n]
