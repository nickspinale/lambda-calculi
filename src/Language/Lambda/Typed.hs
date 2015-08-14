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

data Context = Nil | Cons Type Context
  deriving Show

data Type = O | F Type Type
  deriving Show

-- Finite totally ordered sets, indexed by the naturals
data Fin c where
    Zero :: Proxy t -> Fin (Cons t c)
    Succ :: Proxy t -> Fin c -> Fin (Cons t c)

-- Why does ghc require this to be a standalone instance declaration?
deriving instance Eq (Fin c)
deriving instance Show (Fin c)

-- De Bruijn indexed typed lambda terms
data M c where
    V :: Fin c -> M c
    A :: M c -> M c -> M c
    L :: Proxy t -> M (Cons t c)

deriving instance Eq (M c)
deriving instance Show (M c)
