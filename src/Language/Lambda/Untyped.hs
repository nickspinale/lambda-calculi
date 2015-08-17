{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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
    Zero :: Fin (S n)
    Succ :: Fin n -> Fin (S n)

data Fins p n = Pos (Fin p) | Neg (Fin n)

-- fin :: FiniF p Z -> Fin (S p)
-- fin (Pos p) = Succ p
-- fin Nil = Zero

-- finif :: Fin (S p) -> FiniF p Z
-- finif (Succ p) = Pos p
-- finif Zero = Nil

right :: Fins (S p) n -> Fins p (S n)
right (Pos (Succ p)) = Pos p
right (Pos Zero) = Neg Zero
right (Neg n) = Neg $ Succ n

left :: Fins p (S n) -> Fins (S p) n
left (Pos p) = Pos $ Succ p
left (Neg Zero) = Pos Zero
left (Neg (Succ n)) = Neg n

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
type M = MM Fin

data MM f n = V (f n)
            | A (MM f n) (MM f n)
            | L (MM f (S n))

mmap :: (forall n. f n -> g n) -> MM f n -> MM g n
mmap a m = case m of
    V x -> V (a x)
    A f x -> A (mmap a f) (mmap a x)
    L body -> L $ mmap a body

bury :: M n -> M (S n)
bury m = case m of
    V n -> V $ lift n
    A f x -> A (bury f) (bury x)
    L body -> L (bury body)

compare' :: Nat -> Fin n -> Ordering
compare' (S n) (Succ m) = compare' n m
compare' Z Zero = EQ
compare' _ Zero = GT
compare' Z _    = LT

type family Plus (n :: Nat) (m :: Nat) :: Nat
type instance Plus n Z = n
type instance Plus n (S m) = Plus (S n) m

class G a b where
    g :: Fins a b -> Fin (Plus a b)
    g :: Fins a b -> Fin (Plus a b)

instance G Z Z where
    g = undefined

instance G (S a) Z where
    g (Pos p) = p

instance G (S a) (S b) where
    g (Pos 

instance G (S a b where
    g = undefined

-- g :: Fins a b -> Fin (Plus a b)
-- g (Pos 

substitute :: M n -> M (S n) -> M n
substitute x' body' = undefined
  where
    go :: M n -> MM (Fins n) (S m) -> M n
    go = undefined
    -- go x body = case body of
        -- V (Neg Zero) -> V x
        -- V (Neg (Succ n)) -> g (Neg n)
        -- V (Pos p) -> g (Pos p)
    -- V Zero -> x'
    -- V (Succ n) -> V n
    -- A f x -> A (substitute x' f) (substitute x' x)
    -- L body -> L (substitute (bury x') body)

-- substitute :: forall n. Nat -> M n -> M (S n) -> M n
-- substitute depth x' body' = case body' of
--     V Zero -> x'
--     V (Succ n) -> V n
--     -- V n -> case compare' depth n of
--     --         EQ -> body
--     --         GT -> 
--            -- let go :: forall m. Nat -> Fin (S m) -> Maybe (Fin m)
--            --     go = undefined
--            --     -- go Z Zero = Nothing
--            --     -- go Z (Succ m) = Just m
--            --     -- go (S n) Zero = Just (Zero :: Fin m)
--            --     -- go (S n) (Succ m) = fmap Succ $ go n m
--            -- in maybe x' V (go depth n)
--     A f x -> A (substitute depth x' f) (substitute depth x' x)
--     L body -> L (substitute (S depth) (bury x') body)

-- class Sub (n :: Nat) (m :: Nat) where

beta :: M n -> Maybe (M n)
beta m = case m of
    A (L body) x -> Just $ substitute x body
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
