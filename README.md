# lambda-calculi

Nifty type-safe implementations of various lambda calculi (currently just untyped and simply typed).

Finite totally ordered sets are used for De Bruijn indexing:

```haskell
data Fin n where
    Zero :: Fin (S n)
    Succ :: Fin n -> Fin (S n)
```

This project is currently on hold, but may someday be used in the implementation of a crappy toy dependantly typed language. We'll see.
