
module Main (main) where

import           Test.HUnit
import           Tests      (tests)

main = runTestTT tests
