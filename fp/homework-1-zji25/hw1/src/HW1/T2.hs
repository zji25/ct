module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import           Data.Maybe      (fromJust)
import           Numeric.Natural

data N = Z | S N

nplus :: N -> N -> N
nplus a b = case (a, b) of
  (Z, _)       -> b
  (_, Z)       -> a
  (S a', S b') -> S $ S $ nplus a' b'

nmult :: N -> N -> N
nmult a b = case (a, b) of
  (_, Z)    -> Z
  (Z, _)    -> Z
  (_, S b') -> nplus a (nmult a b')

nsub :: N -> N -> Maybe N
nsub a b = case (a, b) of
  (a', Z)      -> Just a'
  (Z, _)       -> Nothing
  (S a', S b') -> nsub a' b'

ncmp :: N -> N -> Ordering
ncmp a b = case (a, b) of
  (Z, Z)       -> EQ
  (Z, _)       -> LT
  (_, Z)       -> GT
  (S a', S b') -> ncmp a' b'

nFromNatural :: Natural -> N
nFromNatural n = (iterate S Z) !! fromIntegral n -- ok because we're generating infinite list

nToNum :: Num a => N -> a
nToNum n = case n of
  Z      -> 0
  (S n') -> nToNum n' + 1

nEven :: N -> Bool
nEven n = case n of
  Z          -> True
  (S (S n')) -> nEven n'
  _          -> False

nOdd :: N -> Bool
nOdd = not . nEven

ndiv :: N -> N -> N
ndiv _ Z = undefined
ndiv a b
  | ncmp a b == LT = Z
  | otherwise      = S $ ndiv (fromJust (nsub a b)) b -- just checked that a >= b so nsub will fr return Just

nmod :: N -> N -> N
nmod _ Z = undefined
nmod a b
  | ncmp a b == LT = a
  | otherwise      = nmod (fromJust (nsub a b)) b -- just checked that a >= b so nsub will fr return Just

