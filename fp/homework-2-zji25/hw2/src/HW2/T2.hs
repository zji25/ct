module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty(..))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn separator = foldr f ([] :| [])
  where
    f x (z :| zs)
      | x == separator = [] :| z : zs   -- creating new list to collect elements, previous is complete
      | otherwise      = (x : z) :| zs  -- prepending the element to current list

joinWith :: a -> NonEmpty [a] -> [a]
-- prepending separator in front of each element except first,
-- then flattening list via foldr1 
-- foldr1 requires a nonempty list, good thing we can guarantee it gets it
joinWith separator (x :| xs) = foldr1 (++) (x : (map (separator :) xs))
