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
    ) where

data Nat = Z | S Nat

type family (n :: Nat) + (m :: Nat) :: Nat
type instance n + Z = n
type instance n + (S m) = (S n) + m

-- class KnownNat (n :: Nat) where
--     value :: Proxy n -> Nat

-- instance KnownNat Z where
--     value _ = Z

-- instance KnownNat n => KnownNat (S n) where
--     value _ = S $ value (Proxy :: Proxy n)
