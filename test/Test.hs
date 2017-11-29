import Test.Tasty
import Cases (cases)

main = defaultMain tests

tests = testGroup [] [cases]
