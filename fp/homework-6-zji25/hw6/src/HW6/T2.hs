{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : HW6.T2
Description : type-level set
this module contains an implemention of operations on type-level sets 
using type families.
'TSet' guarantees the uniqueness of its elements

__example:__

@
data Proxy a = Proxy

type ExampleSet = '[ "a", "b" ] :: TSet

test1 :: Proxy 'True
test1 = Proxy @(Contains "a" ExampleSet)

test2 :: Proxy '[ "b" ]
test2 = Proxy @(Delete "a" ExSet)
@
-}

module HW6.T2
  ( TSet
  , Contains
  , Add
  , Delete
  ) where

import Data.Type.Bool (If)
import GHC.TypeLits

-- | type synonym to represent a set of type-level strings
type TSet = [Symbol]

-- | type family for checking if a 'Symbol' is present in a 'TSet'
type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains name '[]           = 'False
  Contains name (name ': set) = 'True
  Contains name (_ ': set)    = Contains name set

-- | type family for removing a 'Symbol' from a 'TSet'
-- 
-- does nothing if a given symbol was not present in a set
type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete name '[]           = '[]
  Delete name (name ': set) = set
  Delete name (x ': set)    = x ': Delete name set

-- | type family for adding a 'Symbol' to a 'TSet'
-- 
-- does nothing if a given symbol was already present in a set
type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add v set = If (Contains v set) set (v ': set)
