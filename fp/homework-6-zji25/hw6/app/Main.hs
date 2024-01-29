{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Ord.HT (inRange)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Options.Applicative
import Prettyprinter.Render.Terminal (putDoc)

import HW6.T3


main :: IO ()
main = do
  validate <$> execParser opts >>= \case
    Left err -> putStrLn $ "validation error: " ++ show err
    Right options -> do
      seed <- round <$> getPOSIXTime
      putStrLn $ "starting simulation"
      mapM_ putDoc $ map (prettyComonad19Grid $ gSize options) 
        $ take (iters options) $ simulate (getConfig options) seed


data Options = Options 
  { prob  :: Double
  , incub :: Int
  , ill   :: Int
  , immun :: Int
  , gSize :: Int
  , iters :: Int
  }

getConfig :: Options -> Config 
getConfig (Options prob' incub' ill' immun' _ _) = Config prob' incub' ill' immun'

data ValidationError = NegativeIntField | ProbabilityOutOfRange
  deriving Show

validate :: Options -> Either ValidationError Options
validate options
  | not $ inRange (0,1) $ prob options = Left ProbabilityOutOfRange
  | any (<1) $ map ($ options) [incub, ill, immun, gSize, iters] = 
      Left NegativeIntField
  | otherwise = Right options

opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
  $ fullDesc
  <> progDesc "simulation of Covid-19 infection on a 2-dimensional grid"
  <> header "Comonad19 Simulation"

optionsParser :: Parser Options
optionsParser = Options
  <$> opt "prob" "INFECTION_PROBABILITY" 
      "infection transmission probability (0 <= p <= 1)"
  <*> opt "incub" "INCUBATION_PERIOD_DURATION"
      "duration of incubation period (days > 0)"
  <*> opt "ill" "ILLNESS_DURATION" "duration of illness (days > 0)"
  <*> opt "immun" "IMMUNITY_DURATION" "duration of immunity (days > 0)"
  <*> opt "grid-size" "GRIDSIZE" "output grid size (n > 0)"
  <*> opt "iterations" "ITERATIONS" "number of simulation iterations (n > 0)"
  where
    opt arg meta hlp = option auto $ long arg <> metavar meta <> help hlp
