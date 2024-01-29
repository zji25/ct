{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}

module HW5.Base
  ( HiError(..)
  , HiExpr(..)
  , HiFun(..)
  , HiValue(..)
  , HiAction(..)
  , HiMonad(..)
  , HiShow(..)
  , hiFunNames
  ) where

import Codec.Serialise (Serialise)
import Data.ByteString (ByteString, unpack)
import Data.List (intercalate, sort)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Numeric (showHex)

data HiFun =
  HiFunAdd
  | HiFunSub
  | HiFunMul
  | HiFunDiv
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Eq, Enum, Bounded, Generic, Serialise)

data HiValue =
  HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Generic, Serialise)

data HiExpr =
  HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show)

data HiError =
  HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show)

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Eq, Generic, Serialise)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

instance Show HiFun where
  show HiFunAdd            = "add"
  show HiFunSub            = "sub"
  show HiFunMul            = "mul"
  show HiFunDiv            = "div"
  show HiFunNot            = "not"
  show HiFunAnd            = "and"
  show HiFunOr             = "or"
  show HiFunLessThan       = "less-than"
  show HiFunGreaterThan    = "greater-than"
  show HiFunEquals         = "equals"
  show HiFunNotLessThan    = "not-less-than"
  show HiFunNotGreaterThan = "not-greater-than"
  show HiFunNotEquals      = "not-equals"
  show HiFunIf             = "if"
  show HiFunLength         = "length"
  show HiFunToUpper        = "to-upper"
  show HiFunToLower        = "to-lower"
  show HiFunReverse        = "reverse"
  show HiFunTrim           = "trim"
  show HiFunList           = "list"
  show HiFunRange          = "range"
  show HiFunFold           = "fold"
  show HiFunPackBytes      = "pack-bytes"
  show HiFunUnpackBytes    = "unpack-bytes"
  show HiFunEncodeUtf8     = "encode-utf8"
  show HiFunDecodeUtf8     = "decode-utf8"
  show HiFunZip            = "zip"
  show HiFunUnzip          = "unzip"
  show HiFunSerialise      = "serialise"
  show HiFunDeserialise    = "deserialise"
  show HiFunRead           = "read"
  show HiFunWrite          = "write"
  show HiFunMkDir          = "mkdir"
  show HiFunChDir          = "cd"
  show HiFunParseTime      = "parse-time"
  show HiFunRand           = "rand"
  show HiFunEcho           = "echo"
  show HiFunCount          = "count"
  show HiFunKeys           = "keys"
  show HiFunValues         = "values"
  show HiFunInvert         = "invert"

instance Ord HiFun where
  compare x y = compare (show x) (show y)

hiFunNames :: [(HiFun, String)]
hiFunNames = reverse $ sort [(f, show f) | f <- [minBound .. maxBound]]

instance Ord HiValue where
  (<=) (HiValueNumber x) (HiValueNumber y)     = x <= y
  (<=) (HiValueBool x) (HiValueBool y)         = x <= y
  (<=) (HiValueFunction x) (HiValueFunction y) = x <= y
  (<=) (HiValueString x) (HiValueString y)     = x <= y
  (<=) HiValueNumber{} HiValueBool{}           = False
  (<=) HiValueBool{} HiValueNumber{}           = True
  (<=) _ _                                     = True

instance Show HiAction where
  show = \case
    HiActionRead path -> showAsHiFun HiFunRead [show path]
    HiActionWrite path bs -> showAsHiFun HiFunWrite [show path, hiShow bs]
    HiActionMkDir path -> showAsHiFun HiFunMkDir [show path]
    HiActionChDir path -> showAsHiFun HiFunChDir [show path]
    HiActionCwd -> "cwd"
    HiActionNow -> "now"
    HiActionRand x y -> showAsHiFun HiFunRand [show x, show y]
    HiActionEcho txt -> showAsHiFun HiFunEcho [show txt]
    where
      showAsHiFun hf args = (show hf) ++ '(' : intercalate ", " args ++ ")"


class HiShow a where
  hiShow :: a -> String

instance HiShow ByteString where
  hiShow = (++) "[# " . foldr (\w8 acc -> (hex w8) ++ (' ' : acc)) "#]" . unpack
    where
      hex = addLeadingZero . flip showHex ""
      addLeadingZero x = if length x == 1 then '0' : x else x
