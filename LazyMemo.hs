{-# LANGUAGE RankNTypes #-}

module LazyMemo where

import Data.Lub

type Memo a = forall r. (HasLub r) => (a -> r) -> a -> r

bool :: Memo Bool
bool f = const (f undefined) `lub` table (f False) (f True)
  where
  table f t False = f
  table f t True  = t

memo2 :: Memo a -> Memo b -> forall r. (HasLub r) => (a -> b -> r) -> a -> b -> r
memo2 ma mb = ma . (mb .)

list :: Memo a -> Memo [a]
list ma f = const (f undefined) `lub` table (f []) (ma (\x -> list ma (f . (x:))))
  where
  table nil cons [] = nil
  table nil cons (x:xs) = cons x xs
