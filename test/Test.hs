import Test.Tasty
import Cases (cases)

main = defaultMain tests

tests :: TestTree
tests = testGroup [] [cases]
