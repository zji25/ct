module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving Show

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last x) ys = x :+ ys
  (<>) (x :+ xs) ys = x :+ (xs <> ys)

data Inclusive a b = This a | That b | Both a b
  deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This x) (This y)         = This (x <> y)
  (<>) (This x) (That y)         = Both x y
  (<>) (This x) (Both y1 y2)     = Both (x <> y1) y2
  (<>) (That x) (This y)         = Both y x
  (<>) (That x) (That y)         = That (x <> y)
  (<>) (That x) (Both y1 y2)     = Both y1 (x <> y2)
  (<>) (Both x1 x2) (This y)     = Both (x1 <> y) x2
  (<>) (Both x1 x2) (That y)     = Both x1 (x2 <> y)
  (<>) (Both x1 x2) (Both y1 y2) = Both (x1 <> y1) (x2 <> y2)

newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  DS x <> DS y 
    | x == ""   = DS y
    | y == ""   = DS x
    | otherwise = DS $ x ++ ('.' : y)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F f <> F g = F $ f . g

instance Monoid (Fun a) where
  mempty = F id
