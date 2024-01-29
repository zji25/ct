-- | this module defines 'ListZipper' datatype
module Data.ListZipper
  ( ListZipper (..)
  , lGenerator
  , lLeft
  , lRight
  , toList
  ) where

import Control.Comonad (Comonad (..))

-- | the 'ListZipper' datatype represents a focused element in an infinite 
-- list with elements on its left and right
data ListZipper a = LZ [a] a [a]

instance Functor ListZipper where
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Comonad ListZipper where
  extract (LZ _ x _) = x
  extend f = fmap f . lGenerator lLeft lRight

-- | creates a 'ListZipper' around an element using two shifting functions
-- 
-- returns a 'ListZipper' with the specified element as its focus element 
-- and infinite lists of repeated applications of the provided left and right 
-- shifting functions to the focus element as elements on focus' left and 
-- right respectively
lGenerator :: (a -> a) -> (a -> a) -> a -> ListZipper a
lGenerator fl fr x = LZ (iterateTail fl x) x (iterateTail fr x)
  where iterateTail f = tail . iterate f

-- | shifts the focus of the 'ListZipper' to the element on the left
lLeft :: ListZipper a -> ListZipper a
lLeft (LZ (l:ls) x rs) = LZ ls l (x:rs)
lLeft lz               = lz

-- | shifts the focus of the 'ListZipper' to the element on the right
lRight :: ListZipper a -> ListZipper a
lRight (LZ ls x (r:rs)) = LZ (x:ls) r rs
lRight lz               = lz

-- | 'toList' converts a 'ListZipper' to a list of a specified size, 
-- with the focused element in a center
toList :: Int -> ListZipper a -> [a]
toList size (LZ ls x rs) 
  | size > 0  = reverse (take nl ls) ++ x : take nr rs
  | otherwise = []
  where
    (nl,nr) | even size = (size'-1, size')
            | otherwise = (size', size')
    size' = size `div` 2
