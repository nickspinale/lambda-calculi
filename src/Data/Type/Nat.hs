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

module Data.Type.Nat
    ( Nat(..)
    , type (+)
    , Value(..)
    ) where

import Data.Proxy

data Nat = Z | S Nat

type family (n :: Nat) + (m :: Nat) :: Nat
type instance n + Z = n
type instance n + (S m) = (S n) + m

-- Links the type- and value-level
class Value (n :: Nat) where
    value :: Proxy n -> Nat

instance Value Z where
    value _ = Z

instance Value n => Value (S n) where
    value _ = S $ value (Proxy :: Proxy n)
