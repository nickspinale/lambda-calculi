{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda
    (
    ) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8

-- Naturals for use on the type-level
data Nat = Z | S Nat

-- Finite totally ordered sets, indexed by the naturals
data Fin (n :: Nat) where
    Zero :: Fin (S n)
    Succ :: Fin n -> Fin (S n)

value :: Fin n -> Integer
value Zero = 0
value (Succ n) = value n

instance Show (Fin n) where
    show = show . value

-- De Bruijn indexed lambda expressions, where variables are labeled with t's
data Term (n :: Nat) = Variable (Fin n)
                     | Application (Term n) (Term n)
                     | Abstraction (Term (S n))
  deriving Show

data Expr = Variable' Identifier
          | Application' Expr Expr
          | Abstraction' Identifier Expr
  deriving Show

newtype Identifier = Identifier { unwrap :: String }
  deriving (Eq, Show)

expr :: Parser Expr
expr = char '(' *> expr' <* char ')'
  where
    expr' :: Parser Expr
    expr' = Variable' <$> identifier
         <|> Application' <$> expr <*> expr
         <|> Abstraction' <$> (string "\\ " *> identifier <* string " . ") <*> expr'
    identifier :: Parser Identifier
    identifier = Identifier <$> many1 letter_ascii
