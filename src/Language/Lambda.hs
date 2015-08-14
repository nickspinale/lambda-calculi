{-# LANGUAGE GADTs #-}

module Data.Fin
    ( 
    ) where

data Zero
data Succ n

data Fin n where
    Zero :: Fin n
    Succ :: Fin n -> Fin (Succ n)

data Expr n = Variable (Fin n)
            | Application (Expr n) (Expr n)
            | Abstraction (Expr (Succ n))
