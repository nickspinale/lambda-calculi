{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Lambda.Untyped
    (
    ) where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Builder
import Data.Proxy
import Control.Applicative

data Nat = Z | S Nat
  deriving Show

-- Links the type- and value-level
class Value (n :: Nat) where
    value :: Proxy n -> Nat

instance Value Z where
    value _ = Z

instance Value n => Value (S n) where
    value _ = S $ value (Proxy :: Proxy n)


-- Finite totally ordered sets, indexed by the naturals
data Fin n where
    Zero :: Fin (S n)
    Succ :: Fin n -> Fin (S n)

-- Why does ghc require this to be a standalone instance declaration?
deriving instance Show (Fin n)

expand :: Fin n -> Nat
expand Zero = Z
expand (Succ n) = S $ expand n

-- instance Show (Fin n) where
--     show = show . expand

-- De Bruijn indexed untyped lambda terms
data M n = V (Fin n)
         | A (M n) (M n)
         | L (M (S n))
  deriving Show

data M' = V' Id
        | A' M' M'
        | L' Id M'
  deriving Show

newtype Id = Id { unwrap :: String }
  deriving (Eq, Show)

identifier :: Parser Id
identifier = Id <$> many1 letter_ascii

m' :: Parser M'
m' = V' <$> identifier
  <|> parens (A' <$> m' <* space <*> m')
  <|> parens (L' <$> (string "\\ " *> identifier <* string " . ") <*> m')
  where
    parens :: Parser a -> Parser a
    parens a = char '(' *> a <* char ')'

pprint :: M' -> Builder
pprint = undefined

dumb :: M Z -> M'
dumb = undefined

check :: M' -> Either String (M Z)
check = undefined

beta :: M Z -> M Z
beta = undefined
