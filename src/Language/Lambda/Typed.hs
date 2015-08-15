{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Lambda.Typed
    (
    ) where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Builder
import Data.Proxy
import Control.Applicative

-- A list of abstraction types, for the type level
data Context = Nil | Cons Type Context
  deriving Show

-- The type of terms.
data Type = O | F Type Type
  deriving Show

-- Finite totally ordered sets, but with type information.
-- Sorry about not having a better description.
data Fin (t :: Type) (c :: Context) where
    Zero :: Fin t (Cons t c)
    Succ :: Proxy t1 -> Fin t0 c -> Fin t0 (Cons t1 c)

-- De Bruijn indexed typed lambda terms
data M (t :: Type) (c :: Context) where
    V :: Fin t c -> M t c
    A :: M (F a b) c -> M a c -> M b c
    L :: M t1 (Cons t1 c) -> M (F t0 t1) c

-- deriving instance Eq (Fin c)
-- deriving instance Show (Fin c)
-- deriving instance Eq (M t c)
-- deriving instance Show (M t c)
