{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module LazyNaturals where

import Data.Lub
import Data.Glb
import Data.Unamb
import Data.Repr

data Nat = Z | S Nat
  deriving (Eq, Show)

instance HasRepr Nat (Maybe Nat) where
  repr Z = Nothing
  repr (S x) = Just x

  unrepr Nothing = Z
  unrepr (Just x) = S x

instance HasLub Nat where
  lub = onRepr2 lub

instance HasGlb Nat where
  glb = onRepr2 glb

atLeast :: Integer -> Nat
atLeast 0 = undefined
atLeast n = S (atLeast (n-1))

isAtLeast :: Integer -> Nat -> Bool
isAtLeast 0 _ = True
isAtLeast n Z = False
isAtLeast n (S x) = isAtLeast (n-1) x

strictPlus :: Nat -> Nat -> Nat
strictPlus Z y = y
strictPlus (S x) y = S (strictPlus x y)

plus1 :: Nat -> Nat -> Nat
x `plus1` y = (x `strictPlus` y) `lub` (y `strictPlus` x)
-- atLeast 3 `plus` atLeast 2 = atLeast 3
-- we want  = atLeast 5

infix 1 |>
(|>) = seq

plus2 :: Nat -> Nat -> Nat
plus2 x y = unambs [
  case x of
    Z -> y
    S x -> S (plus2 x y),
  case y of
    Z -> x
    S y -> S (plus2 x y)
  ]
-- this one works pretty good, and fast too

countEm :: Nat -> [Integer]
countEm = go 0
  where
  go n Z = [n]
  go n (S x) = n `seq` (n : go (n+1) x)

times :: Nat -> Nat -> Nat
times x y = unambs [
  case x of
    Z -> Z
    S x -> (x `times` y) `plus2` y,
  case y of
    Z -> Z
    S y -> x `plus2` (x `times` y)
  ]
-- this one works well & fast also



pcase :: (HasLub a, HasGlb a) => a -> (Nat -> a) -> Nat -> a
pcase z s n = (z `glb` s undefined) `lub` (case n of Z -> z; S x -> s x)

plus3 :: Nat -> Nat -> Nat
plus3 x y = pcase y (\x' -> S (plus3 x' y)) x
-- this gives the correct result!  It's slow though.


prec :: (HasLub a, HasGlb a) => a -> (a -> a) -> Nat -> a
prec z s n = unambs [
  case n of
    Z   -> z
    S n -> s (prec z s n),
  pcase z (s . prec z s) n
  ]

plus4 :: Nat -> Nat -> Nat
plus4 x y = prec y S x
-- seems to be correct and faster.

-- plus4 (S (S _|_)) (S (S _|_))
-- prec (S (S _|_)) S (S (S _|_))
-- S (prec (S (S _|_)) S (S _|_))
-- S (S (prec (S (S _|_)) S _|_))
-- S (S (pcase (S (S _|_)) (S . prec (S (S _|_)) S) _|_))
-- S (S (((S (S _|_)) `glb` (S (prec (S (S _|_)) S _|_))) `lub` _|_))
-- S (S ((S (S _|_)) `glb` (S (prec (S (S _|_)) S _|_))))
-- S (S (S (S _|_ `glb` prec (S (S _|_)) S _|_)))
-- S (S (S (S _|_ `glb` pcase (S (S _|_)) (S . prec (S (S _|_))) _|_)))
-- S (S (S (S _|_ `glb` ((S (S _|_) `glb` (S (prec (S (S _|_)) _|_))) `lub` _|_))))
-- S (S (S (S _|_ `glb` (S (S _|_) `glb` (S (prec (S (S _|_)) _|_))))))
-- S (S (S (S _|_ `glb` (S (S _|_ `glb` (prec (S (S _|_)) _|_))))))
-- S (S (S (S (_|_ `glb` (S _|_ `glb` (prec (S (S _|_)) _|_))))))
-- S (S (S (S _|_)))


