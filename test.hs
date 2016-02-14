
import Test.HUnit
import H_99.H_01_10
import H_99.H_11_20
import H_99.H_21_28
import H_99.H_31_41

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

tests =
  [test01, test02, test03, test04, test05,
   test06, test07, test08, test09, test10,
   test11, test12, test13, test14, test15,
   test16, test17, test18, test19, test20,
   test21, test22, test26, test27, test28,
   test31, test32, test33, test34, test35,
   test36, test37, test38, test39, test40,
   test41]

testlist = TestList tests

rt = runTestTT testlist
