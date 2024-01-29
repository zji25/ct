{-# LANGUAGE LambdaCase #-}

module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Numeric.Natural (Natural)
import Data.Function (fix)

repeat' :: a -> [a]
repeat' x = fix (x :) 

map' :: (a -> b) -> [a] -> [b]
map' f = fix $ \r -> \case
  []     -> []
  (x:xs) -> f x : r xs

fib :: Natural -> Natural
fib = fix f 0 1
  where 
    f r a b = \case 
      0 -> a 
      n -> r b (a+b) (n-1)

fac :: Natural -> Natural
fac = fix $ \r n -> if n <= 1 then 1 else n * r (n-1)
