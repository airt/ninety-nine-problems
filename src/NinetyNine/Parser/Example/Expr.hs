module NinetyNine.Parser.Example.Expr where

import Control.Applicative ((<|>))
import NinetyNine.Parser.Char
import NinetyNine.Parser.Combinator
import NinetyNine.Parser.Prim
import NinetyNine.Parser.Token

parseExpr :: String -> Maybe Double
parseExpr = parse $ completes expr

expr = lexeme $ chainl1 term addop

term = lexeme $ chainl1 factor mulop

factor = lexeme $ parens (spaces *> expr) <|> number

addop = lexeme $ (+) <$ char '+' <|> (-) <$ char '-'

mulop = lexeme $ (*) <$ char '*' <|> (/) <$ char '/'

number :: Parser Double
number = float <|> (fromInteger <$> integer)
