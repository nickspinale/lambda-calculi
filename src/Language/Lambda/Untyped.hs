{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Lambda.Untyped
    (
    ) where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Builder as B
import           Data.Proxy
import           Control.Applicative
import           Control.Monad
import           Data.Monoid

data Nat = Z | S Nat
  deriving (Ord, Eq, Show)

-- instance Num Nat where
--     n + Z = n
--     n + S m = S n + m
--     n - Z = n
--     S n - S m = n - m
--     Z - S _ = error "can't subtract S _ from Z"
--     n * Z = Z
--     n * S m = n + n * m
--     abs = id
--     signum Z = 0
--     signum _ = 1
--     fromInteger 0 = Z
--     fromInteger n | n < 0 = error (show n ++ " is not a natural number")
--                   | otherwise = S . fromInteger $ n - 1

-- instance Integral Nat where
--     toInteger Z = 0
--     toInteger (S n) = 1 + toInteger n

-- instance Enum Nat where

-- instance Real Nat where

-- Links the type- and value-level
class Value (n :: Nat) where
    value :: Proxy n -> Nat

instance Value Z where
    value _ = Z

instance Value n => Value (S n) where
    value _ = S $ value (Proxy :: Proxy n)

-- Finite totally ordered sets, indexed by the naturals
data Fin n where
    Zero  :: Fin (S n)
    Succ :: Fin n -> Fin (S n)

lift :: Fin n -> Fin (S n)
lift Zero = Zero
lift (Succ n) = Succ (lift n)

-- Why does ghc require this to be a standalone instance declaration?
deriving instance Show (Fin n)

-- say :: forall n. Value n => Fin n -> Integer
-- say n = toInteger $ value (Proxy :: Proxy n) - expand n

expand :: Fin n -> Nat
expand Zero = Z
expand (Succ x) = S $ expand x

-- instance Show (Fin n) where
--     show = show . expand

-- De Bruijn indexed untyped lambda terms
data M n = V (Fin n)
         | A (M n) (M n)
         | L (M (S n))
  deriving Show

bury :: M n -> M (S n)
bury m = case m of
    V n -> V $ lift n
    A f x -> A (bury f) (bury x)
    L body -> L (bury body)

substitute :: forall n. Nat -> M n -> M (S n) -> M n
substitute depth x' body' = case body' of
    V n -> let go :: Nat -> Fin (S m) -> Maybe (M m)
               go Z Zero = Nothing
               go Z (Succ m) = Just $ V m
               go (S n) Zero = Just $ V Zero
               go (S n) (Succ m) = go n m
           in maybe x' id (go depth n)
    A f x -> A (substitute depth x' f) (substitute depth x' x)
    L body -> L (substitute (S depth) (bury x') body)

-- class Sub (n :: Nat) (m :: Nat) where

beta :: M n -> Maybe (M n)
beta m = case m of
    A (L body) x -> Just $ substitute Z x body
    L body -> fmap L $ beta body
    _ -> Nothing

data M' = V' Id
        | A' M' M'
        | L' Id M'
  deriving Show

newtype Id = Id { unwrap :: String }
  deriving (Eq, Show)

identifier :: Parser Id
identifier = Id <$> many1 letter_ascii

identifier' :: Id -> B.Builder
identifier' = B.string8 . unwrap

m' :: Parser M'
m' = (V' <$> identifier)
  <|> (parens (A' <$> m' <* space <*> m'))
  <|> (parens (L' <$> (char '\\' *> identifier <* char '.') <*> m'))
  where
    parens :: Parser a -> Parser a
    parens a = char '(' *> a <* char ')'

pprint' :: M' -> B.Builder
pprint' m = case m of
    V' id -> identifier' id
    A' x y -> parens $ pprint' x <> B.char8 ' ' <> pprint' y
    L' id body -> parens $ B.char8 '\\' <> identifier' id <> B.char8 '.' <> pprint' body
  where
    parens :: B.Builder -> B.Builder
    parens a = B.char8 '(' <> a <> B.char8 ')'

dumb :: M Z -> M'
dumb = undefined

check :: M' -> Either String (M Z)
check = undefined


-- Testing -----------------------------------

y = "(\\f.(\\x.(f (x x))) (\\x.(f (x x))))"
