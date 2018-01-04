{-# LANGUAGE LambdaCase #-}

module NinetyNine.Parser.Char where

import Control.Applicative (empty, many)
import Control.Monad.State.Lazy (get, put)
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Functor (void)
import NinetyNine.Parser.Prim

oneOf :: String -> Parser Char
oneOf cs = satisfy (`elem` cs)

noneOf :: String -> Parser Char
noneOf cs = satisfy (`notElem` cs)

spaces :: Parser ()
spaces = void (many space)

space :: Parser Char
space = satisfy isSpace

letter :: Parser Char
letter = satisfy isAlpha

digit :: Parser Char
digit = satisfy isDigit

char :: Char -> Parser Char
char = satisfy . (==)

anyChar :: Parser Char
anyChar = get >>= \case
  x : xs -> put xs >> return x
  _ -> empty

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = anyChar >>= \x -> if p x then return x else empty

string :: String -> Parser String
string = mapM char
