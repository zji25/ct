-- | this module defines 'Grid' datatype
module Data.Grid
  ( Grid (..)
  , gNeighbours
  , gUpdate
  ) where

import Control.Comonad (Comonad (..))
import Control.Monad (liftM2)

import Data.ListZipper (ListZipper (..), lGenerator, lLeft, lRight)

-- | datatype representing an infinite 2-dimensional grid using a 
-- 'ListZipper' of 'ListZipper's
--
-- the focus element of the grid is the focus element of focused grid's 
-- listzipper
newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

instance Functor Grid where
  fmap f = Grid . fmap (fmap f) . unGrid

instance Comonad Grid where
  extract = extract . extract . unGrid
  extend f = fmap f . Grid . fmap gHorizontal . gVertical


gHorizontal, gVertical :: Grid a -> ListZipper (Grid a)
gHorizontal = lGenerator gLeft gRight
gVertical   = lGenerator gUp gDown

gLeft, gRight, gUp, gDown :: Grid a -> Grid a
gLeft  = gMove $ fmap lLeft
gRight = gMove $ fmap lRight
gUp    = gMove lLeft
gDown  = gMove lRight

gMove :: (ListZipper (ListZipper a) -> ListZipper (ListZipper a)) -> Grid a -> Grid a
gMove f = Grid . f . unGrid

-- | list of 8 functions, each moving a focus element of the grid one step in 
-- different directions: left, right, up, down or diagonally
gNeighbours :: [Grid a -> Grid a]
gNeighbours = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where
    horizontals = [gLeft, gRight]
    verticals   = [gUp, gDown]

-- | updates the grid by applying provided function to the focused element 
-- of the grid
gUpdate :: (a -> a) -> Grid a -> Grid a
gUpdate f (Grid (LZ ls (LZ xls x xrs) rs)) = Grid $ LZ ls (LZ xls (f x) xrs) rs
