module NinetyNine.Parser.Combinator where

import Control.Applicative ((<|>))
import Control.Monad.State.Lazy (gets, guard)
import Data.Functor (void)
import NinetyNine.Parser.Prim

between :: Parser o -> Parser c -> Parser a -> Parser a
between open close p = open *> p <* close

option :: a -> Parser a -> Parser a
option x p = p <|> return x

optional :: Parser a -> Parser ()
optional p = void p <|> return ()

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = next x <|> return x
    next x = do
      f <- op
      y <- p
      rest (f x y)

eof :: Parser ()
eof = gets null >>= guard
