module NinetyNine.Parser.Example.ExprSpec where

import Test.Hspec
import NinetyNine.Parser

spec :: Spec
spec = describe "Expr" $ do

  it "single" $ do
    parseExpr "" `shouldBe` Nothing
    parseExpr "- 10" `shouldBe` Nothing
    parseExpr " 10   " `shouldBe` Just 10
    parseExpr "-10   " `shouldBe` Just (-10)
    parseExpr " 10.01" `shouldBe` Just 10.01
    parseExpr "-10.01" `shouldBe` Just (-10.01)

  it "simple" $ do
    parseExpr "1 + 2 " `shouldBe` Just 3
    parseExpr "1 - 2 " `shouldBe` Just (-1)
    parseExpr "1 + -2" `shouldBe` Just (-1)
    parseExpr "1 - -2" `shouldBe` Just 3

  it "parens" $ do
    parseExpr "( 1  " `shouldBe` Nothing
    parseExpr "  1 )" `shouldBe` Nothing
    parseExpr " (1) " `shouldBe` Just 1
    parseExpr "( 1 )" `shouldBe` Just 1
    parseExpr "( 1 * ( 1 + 1 ) )" `shouldBe` Just 2

  it "multiple" $ do
    parseExpr "3 / 2 + -1" `shouldBe` Just 0.5
    parseExpr "6 * -5 * 4 / -3 - 2 * -1" `shouldBe` Just 42
