{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Fin
    ( Fin(..)
    , Fins(..)
    , Glueable
    , shift
    , right
    , left
    , lift
    , expand
    ) where

import Data.Type.Nat

-- Finite totally ordered sets, indexed by the naturals
data Fin n where
    Zero :: Fin (S n)
    Succ :: Fin n -> Fin (S n)

-- Why does ghc require this to be a standalone instance declaration?
deriving instance Show (Fin n)

data Fins p n = Pos (Fin p) | Neg (Fin n)

class Glueable a b where
    glue :: Fins a b -> Fin (a + b)

instance Glueable a Z where
    glue (Pos p) = p

instance Glueable (S a) b => Glueable a (S b) where
    glue (Pos p)        = glue (Pos (Succ p) :: Fins (S a) b)
    glue (Neg Zero)     = glue (Pos Zero :: Fins (S a) b)
    glue (Neg (Succ n)) = glue (Neg n :: Fins (S a) b)

shift :: ((a + b) ~ (c + d)) => Fins a b -> Fins c d
shift = undefined

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

expand :: Fin n -> Nat
expand Zero = Z
expand (Succ x) = S $ expand x

