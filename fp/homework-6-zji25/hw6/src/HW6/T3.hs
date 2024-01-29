{-# LANGUAGE LambdaCase #-}

{-|
Module      : HW6.T3
Description : simulation of Covid-19 infection on a 2-dimensional grid
this module provides functionalities to simulate the spread of infection 
on a grid using a comonadic structure. it defines types and functions for 
configuring the simulation, evolving the grid, and visualizing the state 
of the grid
-}

module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid

  , simulate
  , prettyComonad19Grid
  ) where

import Control.Comonad (Comonad (..))
import DSP.Basic (uninterleave)
import Prettyprinter (Doc, Pretty (..), annotate, hcat, line', vsep)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), colorDull)
import System.Random (StdGen, mkStdGen, randomR, split)

import Data.Grid (Grid (..), gNeighbours, gUpdate)
import Data.ListZipper (ListZipper (..), toList)


-- | configuration for the infection simulation
data Config = Config
  { probability      :: Double -- ^ infection transmission probability
  , incubationPeriod :: Int    -- ^ duration of incubation period
  , illnessDuration  :: Int    -- ^ duration of illness
  , immunityDuration :: Int    -- ^ duration of immunity
  }

-- | 'CellState' datatype represents the state of a cell in the grid
data CellState
  = Healthy      -- ^ healthy state
  | Infected Int -- ^ infected state that will last for 
                 -- the specified amount more days
  | Ill Int      -- ^ ill state that will last for the 
                 -- specified amount more days
  | Immune Int   -- ^ immune state that will last for the 
                 -- specified amount more days

-- | 'Cell' datatype represents a cell in the grid
data Cell = Cell
  { cellState :: CellState -- ^ cell's current state
  , cellRand  :: StdGen    -- ^ cell's random generator
  }

-- | 'Comonad19Grid' represents an infinite grid of 'Cell's
type Comonad19Grid = Grid Cell

-- | creates an infinite list of grids using the given configuration
--
-- each element of this list represents one infection simulation step
-- 
-- first element always contains one infected cell, other cells remain healthy
simulate 
  :: Config          -- ^ configuration for the infection simulation
  -> Int             -- ^ initial seed for random
  -> [Comonad19Grid] -- ^ simulation result
simulate config seed = iterate (extend $ evolve config) infectedGrid
  where
    infiniteGens gen = let (g1, g2) = split gen in g1 : infiniteGens g2
    infiniteCells = map (Cell Healthy) $ infiniteGens $ mkStdGen seed
    x = head infiniteCells
    (ls,rs) = uninterleave $ tail infiniteCells
    healthyGrid = Grid $ duplicate $ LZ ls x rs
    infectedGrid = gUpdate 
      (updateState $ Infected $ incubationPeriod config) healthyGrid


evolve :: Config -> Comonad19Grid -> Cell
evolve config grid = case state of
  Healthy    -> maybeInfectedCell
  Infected i -> changeState i $ Ill $ illnessDuration config
  Ill i      -> changeState i $ Immune $ immunityDuration config
  Immune i   -> changeCell i maybeInfectedCell
  where
    cell@(Cell state _) = extract grid
    contagiousCount = length . filter id $ map contagious gNeighbours
    contagious direction = case cellState (extract $ direction grid) of
      Infected _ -> True
      Ill _      -> True
      _          -> False
    -- n infected/ill neighbors, 
    -- probability that none of them infect current cell = (1-p)^n
    -- probability that cell becomes infected = 1-(1-p)^n
    prob = 1-(1-(probability config))^contagiousCount
    maybeInfectedCell = tryInfect prob (incubationPeriod config) cell
    changeState i state' = changeCell i $ updateState state' cell
    changeCell i 
      | i == 0 = id
      | otherwise = const $ updateState (decrementState state) cell


tryInfect :: Double -> Int -> Cell -> Cell
tryInfect prob incub cell@(Cell _ gen)
  | gotInfected = Cell (Infected incub) gen'
  | otherwise = cell { cellRand = gen' }
  where
    (val, gen') = randomR (0.0, 1.0) gen
    gotInfected = val <= prob

decrementState :: CellState -> CellState
decrementState = \case
  Healthy -> Healthy
  Infected i -> Infected $ i-1
  Ill i -> Ill $ i-1
  Immune i -> Immune $ i-1

updateState :: CellState -> Cell -> Cell
updateState state cell = cell { cellState = state }


-- | pretty-prints the 'Comonad19Grid' with the specified size
--
-- 'prettyComonad19Grid' converts an infinite 'Comonad19Grid' into a 'Doc' 
-- 'AnsiStyle' by reducing the grid to a finite size of @n * n@
-- 
-- it displays the visual representation of the grid's state, where each 
-- 'CellState' is represented by a character with specific color:
-- 
-- * healthy cells are represented by '_' (white)
-- * infected cells are represented by \'i\' (yellow)
-- * ill cells are represented by \'#\' (red)
-- * immune cells are represented by \'@\' (blue)
prettyComonad19Grid :: Int -> Comonad19Grid -> Doc AnsiStyle
prettyComonad19Grid size = (<> line') . (<> line') . vsep 
                           . map prettyRow . toList size . unGrid
  where
    prettyRow = hcat . map (prettyState . cellState) . toList size
    prettyState = \case
      Healthy -> color White '_'
      Infected _ -> color Yellow 'i'
      Ill _ -> color Red '#'
      Immune _ -> color Blue '@'
    color clr = annotate (colorDull clr) . pretty
