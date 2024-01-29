module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = (Int, Int)  -- tree's size & height
-- keeping height too to not recalculate in balance

defMeta :: Meta  -- default value
defMeta = (1, 1)

-- basic AVL tree (height balanced)
data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize tree = case tree of
  Leaf              -> 0
  (Branch sh _ _ _) -> fst sh

tdepth :: Tree a -> Int
tdepth tree = case tree of
  Leaf              -> 0
  (Branch sh _ _ _) -> snd sh

tmember :: Ord a => a -> Tree a -> Bool
tmember e tree = case tree of
  Leaf -> False
  (Branch _ l c r)
    | e == c -> True
    | e < c -> tmember e l
    | otherwise -> tmember e r

balance :: Tree a -> Tree a
balance tree = case tree of
  Leaf -> tree
  (Branch _ left center right)
    | bfactor tree > 1 ->
      rotateLeft $ fixMeta $ Branch defMeta left center (fixRight right)
    | bfactor tree < -1 ->
      rotateRight $ fixMeta $ Branch defMeta (fixLeft left) center right
    | otherwise -> fixMeta tree
    where
      bfactor t = case t of
        (Branch _ l _ r) -> tdepth r - tdepth l
        _                -> 0

      fixMeta t = case t of
        (Branch _ l c r) ->
          Branch (1 + tsize l + tsize r, 1 + max (tdepth l) (tdepth r)) l c r
        _                -> t

      rotateRight t = case t of
        (Branch _ (Branch _ ll lc lr) c r) ->
          fixMeta $ Branch defMeta ll lc (fixMeta $ Branch defMeta lr c r)
        _ -> t

      rotateLeft t = case t of
        (Branch _ l c (Branch _ rl rc rr)) ->
          fixMeta $ Branch defMeta (fixMeta $ Branch defMeta l c rl) rc rr
        _ -> t

      fixRight r
        | bfactor r < 0 = rotateRight r
        | otherwise     = r
      
      fixLeft l
        | bfactor l > 0 = rotateLeft l
        | otherwise     = l


tinsert :: Ord a => a -> Tree a -> Tree a
tinsert e tree = case tree of
  Leaf -> Branch defMeta Leaf e Leaf
  t@(Branch _ l c r)
    | e == c -> t
    | e < c -> balance $ Branch defMeta (tinsert e l) c r
    | otherwise -> balance $ Branch defMeta l c (tinsert e r)


tFromList :: Ord a => [a] -> Tree a
tFromList as = case as of
  []     -> Leaf
  (x:xs) -> tinsert x (tFromList xs)