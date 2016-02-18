
import Test.HUnit
import H_99.H_01_10
import H_99.H_11_20
import H_99.H_21_28
import H_99.H_31_41
import H_99.H_46_50
import H_99.H_54_60
import H_99.H_61_69
import H_99.H_70_73

test01 = TestCase $ assertEqual "myLast"
  9
  $ myLast [1..9]

test02 = TestCase $ assertEqual "myButLast"
  8
  $ myButLast [1..9]

test03 = TestCase $ assertEqual "elementAt"
  5
  $ elementAt [1..9] 5

test04 = TestCase $ assertEqual "myLength"
  9
  $ myLength [1..9]

test05 = TestCase $ assertEqual "myReverse"
  [9,8..1]
  $ myReverse [1..9]

test06 = TestCase $ assertEqual "isPalindrome"
  True
  $ isPalindrome $ [1..9] ++ [8,7..1]

test07 = TestCase $ assertEqual "flatten"
  [1,2,3,4,5]
  $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])

test08 = TestCase $ assertEqual "compress"
  "abcade"
  $ compress "aaaabccaadeeee"

test09 = TestCase $ assertEqual "pack"
  ["aaaa","b","cc","aa","d","eeee"]
  $ pack "aaaabccaadeeee"

test10 = TestCase $ assertEqual "encode"
  [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
  $ encode "aaaabccaadeeee"

test11 = TestCase $ assertEqual "encodeModified"
  [Multiple 4 'a',Single 'b',Multiple 2 'c',
   Multiple 2 'a',Single 'd',Multiple 4 'e']
  $ encodeModified "aaaabccaadeeee"

test12 = TestCase $ assertEqual "decodeModified"
  "aaaabccaadeeee"
  $ decodeModified
    [Multiple 4 'a',Single 'b',Multiple 2 'c',
     Multiple 2 'a',Single 'd',Multiple 4 'e']

test13 = TestCase $ assertEqual "encodeDirect"
  [Multiple 4 'a',Single 'b',Multiple 2 'c',
   Multiple 2 'a',Single 'd',Multiple 4 'e']
  $ encodeDirect "aaaabccaadeeee"

test14 = TestCase $ assertEqual "dupli"
  [1,1,2,2,3,3]
  $ dupli [1, 2, 3]

test15 = TestCase $ assertEqual "repli"
  "aaabbbccc"
  $ repli "abc" 3

test16 = TestCase $ assertEqual "dropEvery"
  "abdeghk"
  $ dropEvery "abcdefghik" 3

test17 = TestCase $ assertEqual "split"
  ("abc", "defghik")
  $ split "abcdefghik" 3

test18 = TestCase $ assertEqual "slice"
  "cdefg"
  $ slice ['a','b','c','d','e','f','g','h','i','k'] 3 7

test19 = TestCase $ assertEqual "rotate"
  "ghabcdef"
  $ rotate ['a','b','c','d','e','f','g','h'] (-2)

test20 = TestCase $ assertEqual "removeAt"
  ('b',"acd")
  $ removeAt 2 "abcd"

test21 = TestCase $ assertEqual "insertAt"
  "aXbcd"
  $ insertAt 'X' "abcd" 2

test22 = TestCase $ assertEqual "range"
  [2..7]
  $ range 2 7

test26 = TestCase $ assertEqual "combinations"
  ["abc","abd","abe","abf","acd","ace","acf","ade","adf","aef",
   "bcd","bce","bcf","bde","bdf","bef",
   "cde","cdf","cef",
   "def"]
  $ combinations 3 "abcdef"

test27 = TestCase $ assertEqual "group'"
  1260
  $ length $ group' [2,3,4]
    ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]

test28 = TestCase $ assertEqual "(lsort,lfsort)"
  (["o","de","de","mn","abc","fgh","ijkl"],
   ["ijkl","o","abc","fgh","de","de","mn"])
  $ (lsort ["abc","de","fgh","de","ijkl","mn","o"],
     lfsort ["abc","de","fgh","de","ijkl","mn","o"])

test31 = TestCase $ assertEqual "isPrime"
  [True, False]
  $ map isPrime [7,9]

test32 = TestCase $ assertEqual "myGCD"
  [9,3,3]
  $ zipWith myGCD [36,(-3),(-3)] [63,6,(-6)]

test33 = TestCase $ assertEqual "coprime"
  [True,False,False]
  $ zipWith coprime [35,(-3),(-3)] [64,6,(-6)]

test34 = TestCase $ assertEqual "totient"
  4
  $ totient 10

test35 = TestCase $ assertEqual "primeFactors"
  [3,3,5,7]
  $ primeFactors 315

test36 = TestCase $ assertEqual "primeFactorsMult"
  [(3,2),(5,1),(7,1)]
  $ primeFactorsMult 315

test37 = TestCase $ assertEqual "totient'"
  4
  $ totient' 10

test38 = TestCase $ assertEqual "totient''"
  [4032,4]
  $ [totient 10090, totient' 10]

test39 = TestCase $ assertEqual "primesR"
  [11,13,17,19]
  $ primesR 10 20

test40 = TestCase $ assertEqual "goldbach"
  (5, 23)
  $ goldbach 28

test41 = TestCase $ assertEqual "goldbachList"
  [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
  $ goldbachList 9 20

test46 = TestCase $ assertEqual "table"
  [[True ,True ,True ],
   [True ,False,True ],
   [False,True ,False],
   [False,False,False]]
  $ table (\a b -> (and' a (or' a b)))

test47 = TestCase $ assertEqual "table2"
  [[True ,True ,True ],
   [True ,False,True ],
   [False,True ,False],
   [False,False,False]]
  $ table (\a b -> a `and'` (a `or'` not b))

test48 = TestCase $ assertEqual "tablen"
  [[True ,True ,True ,True],
   [True ,True ,False,True],
   [True ,False,True ,True],
   [True ,False,False,True],
   [False,True ,True ,True],
   [False,True ,False,True],
   [False,False,True ,True],
   [False,False,False,True]]
  $ tablen 3 (\[a,b,c] -> a`and'`(b`or'`c)`equ'`a`and'`b`or'`a`and'`c)

test49 = TestCase $ assertEqual "gray"
  ["000","001","011","010","110","111","101","100"]
  $ gray 3

test50 = TestCase $ assertEqual "huffman"
  [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
  $ huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]

test55 = TestCase $ assertEqual "cbalTree"
  [ Branch 'x' (Branch 'x' Empty Empty)
               (Branch 'x' Empty
                           (Branch 'x' Empty Empty))
  , Branch 'x' (Branch 'x' Empty Empty)
               (Branch 'x' (Branch 'x' Empty Empty)
                           Empty)
  , Branch 'x' (Branch 'x' Empty
                           (Branch 'x' Empty Empty))
               (Branch 'x' Empty Empty)
  , Branch 'x' (Branch 'x' (Branch 'x' Empty Empty)
                           Empty)
               (Branch 'x' Empty Empty)
  ]
  $ cbalTree 4

test56 = TestCase $ assertEqual "symmetric"
  (True,False)
  $ (symmetric (Branch 'x' (Branch 'x' Empty Empty)
                           (Branch 'x' Empty Empty)),
     symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty))

test57 = TestCase $ assertEqual "construct"
  (Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty)
            (Branch 5 Empty (Branch 7 Empty Empty)),
   True,
   True)
  $ (construct [3, 2, 5, 7, 1],
     symmetric . construct $ [5, 3, 18, 1, 4, 12, 21],
     symmetric . construct $ [3, 2, 5, 7, 1])

test58 = TestCase $ assertEqual "symCbalTrees"
  [ Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty))
               (Branch 'x' (Branch 'x' Empty Empty) Empty)
  , Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty)
               (Branch 'x' Empty (Branch 'x' Empty Empty))
  ]
  $ symCbalTrees 5

btree5 = Branch 'a' (Branch 'b' Empty (Branch 'd' Empty Empty))
                    (Branch 'c' (Branch 'e' Empty Empty) Empty)

test61 = TestCase $ assertEqual "countLeaves, leaves"
  (2, ['d', 'e'])
  $ (countLeaves btree5, leaves btree5)

test62 = TestCase $ assertEqual "internals, atLevel"
  (['a', 'b', 'c'], ['b', 'c'])
  $ (internals btree5, atLevel btree5 2)

test63 = TestCase $ assertEqual "completeBinaryTree, isCompleteBinaryTree"
  (Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty)
              (Branch 'x' Empty Empty),
   True)
  $ (completeBinaryTree 4, isCompleteBinaryTree . completeBinaryTree $ 4)

test64 = TestCase $ assertEqual "layout"
  (Branch ('a',(3,1)) (Branch ('b',(1,2)) Empty
                                          (Branch ('d',(2,3)) Empty Empty))
                      (Branch ('c',(5,2)) (Branch ('e',(4,3)) Empty Empty)
                                          Empty))
  $ layout btree5

test67 = TestCase $ assertEqual "treeToString, stringToTree"
  ("a(b(,d),c(e,))", Just btree5)
  $ (treeToString btree5, stringToTree "a(b(,d),c(e,))")

test68 = TestCase $ assertEqual "preorder, inorder"
  ("abdce", "bdaec")
  $ (preorder btree5, inorder btree5)

test69 = TestCase $ assertEqual "tree2ds, ds2tree"
  ("ab.d..ce...", Just btree5)
  $ (tree2ds btree5, ds2tree "ab.d..ce...")

mtree7 =
  Tnode 'a' [ Tnode 'b' [Tnode 'c' []]
            , Tnode 'd' [Tnode 'e' [], Tnode 'f' []]
            , Tnode 'g' []]

test70 = TestCase $ assertEqual "nnodes, mtreeToString, stringToMtree"
  (7, "abc^^de^f^^g^^", Just mtree7)
  $ (nnodes mtree7, mtreeToString mtree7, stringToMtree "abc^^de^f^^g^^")

test71 = TestCase $ assertEqual "ipl"
  9
  $ ipl mtree7

test72 = TestCase $ assertEqual "bottomUp"
  "cbefdga"
  $ bottomUp mtree7

test73 = TestCase $ assertEqual "sexp"
  "(a (b c) (d e f) g)"
  $ sexp mtree7

tests =
  [test01, test02, test03, test04, test05,
   test06, test07, test08, test09, test10,
   test11, test12, test13, test14, test15,
   test16, test17, test18, test19, test20,
   test21, test22, test26, test27, test28,
   test31, test32, test33, test34, test35,
   test36, test37, test38, test39, test40,
   test41, test46, test47, test48, test49,
   test50, test55, test56, test57, test58,
   test61, test62, test63, test64, test67,
   test68, test69, test70, test71, test72,
   test73]

testlist = TestList tests

rt = runTestTT testlist
