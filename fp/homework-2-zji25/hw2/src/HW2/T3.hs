module HW2.T3
  ( epart
  , mcat
  ) where

import Data.Foldable 

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap fold

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap foldEither
  where 
    foldEither (Left a')  = (a', mempty)
    foldEither (Right b') = (mempty, b')
