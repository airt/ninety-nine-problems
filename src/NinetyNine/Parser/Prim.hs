{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NinetyNine.Parser.Prim where

import Control.Applicative (Alternative)
import Control.Monad.State.Lazy (MonadState, StateT, evalStateT, runStateT)

newtype Parser a = Parser { runParser :: StateT String Maybe a }
  deriving (Alternative, Applicative, Functor, Monad, MonadState String)

parse :: Parser a -> String -> Maybe a
parse = evalStateT . runParser

parses :: Parser a -> String -> Maybe (a, String)
parses = runStateT . runParser
