{-# LANGUAGE LambdaCase #-}

module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ x = x

ns :: Nat a -> Nat a
ns n f = f . n f 

nplus :: Nat a -> Nat a -> Nat a
nplus n1 n2 f = n2 f . n1 f 

nmult :: Nat a -> Nat a -> Nat a
nmult n1 n2 f = n2 $ n1 f

nFromNatural :: Natural -> Nat a
nFromNatural = \case
  0 -> nz 
  x -> ns $ nFromNatural $ x-1

nToNum :: Num a => Nat a -> a
nToNum n = n (+1) 0