module NinetyNine.Parser.Token where

import Control.Applicative ((<|>), some)
import NinetyNine.Parser.Char
import NinetyNine.Parser.Combinator
import NinetyNine.Parser.Prim

natural :: Parser Integer
natural = read <$> some digit

integer :: Parser Integer
integer = sign <*> natural

float :: Parser Double
float = sign <*> unsigned
  where
    unsigned = read . concat <$> sequence [some digit, string ".", some digit]

sign :: Num n => Parser (n -> n)
sign = (negate <$ char '-') <|> (id <$ char '+') <|> return id

lexeme :: Parser a -> Parser a
lexeme = (<* spaces)

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

completes :: Parser a -> Parser a
completes = between spaces eof . lexeme
