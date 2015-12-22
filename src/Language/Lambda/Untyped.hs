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

import           Data.Fin
import           Data.Type.Nat

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as C
import           Data.Proxy
import           Control.Applicative
import           Control.Monad
import           Data.Monoid

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

adjust :: Fin (S n) -> Fin (S n) -> Maybe (Fin n)
adjust  Zero     Zero    = Nothing
adjust  Zero    (Succ b) = Just Zero
adjust (Succ a)  Zero    = Just a
adjust (Succ a) (Succ b) = Succ <$> adjust a b

substitute :: M (S n) -> M n -> M n
substitute = go Zero
  where
    go :: Fin (S m) -> M (S m) -> M m -> M m
    go depth body x = case body of
                        V v -> maybe x V $ adjust v depth

-- substitute :: M n -> M (S n) -> M n
-- substitute x' body' = undefined
--   where
--     go :: M n -> MM (Fins n) (S m) -> M n
--     go = undefined
--     -- go x body = case body of
--         -- V (Neg Zero) -> V x
--         -- V (Neg (Succ n)) -> g (Neg n)
--         -- V (Pos p) -> g (Pos p)
--     -- V Zero -> x'
--     -- V (Succ n) -> V n
--     -- A f x -> A (substitute x' f) (substitute x' x)
--     -- L body -> L (substitute (bury x') body)

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
    A (L body) x -> Just $ substitute body x
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
m' =  (V' <$> identifier)
  <|> parens (A' <$> m' <* space <*> m')
  <|> parens (L' <$> (char '\\' *> identifier <* char '.') <*> m')
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

y = "(\\f.((\\x.(f (x x))) (\\x.(f (x x)))))"
