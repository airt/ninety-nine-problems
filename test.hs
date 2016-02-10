
import Test.HUnit
import H_99.H_01_10

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

tests = TestList [
  test01, test02, test03, test04, test05,
  test06, test07, test08, test09, test10]
