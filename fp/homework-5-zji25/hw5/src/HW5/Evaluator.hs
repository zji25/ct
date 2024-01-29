{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HW5.Evaluator (eval) where

import Codec.Compression.Zlib (bestCompression, compressLevel, compressWith, decompress,
                               defaultCompressParams)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Applicative (liftA2)
import Control.Monad (foldM)
import Control.Monad.Except (ExceptT (..), lift, runExceptT, throwError)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import Data.Word (Word8)
import HW5.Base
import Text.Read (readMaybe)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalExpr

type HiExcept m a = ExceptT HiError m a

evalExpr :: HiMonad m => HiExpr -> HiExcept m HiValue
evalExpr (HiExprValue value) = return value
evalExpr (HiExprApply fe xe) = evalExpr fe >>= \fv ->
  case fv of
    HiValueFunction HiFunIf -> applyLazyIf xe
    HiValueFunction HiFunAnd -> applyLazyAnd xe
    HiValueFunction HiFunOr -> applyLazyOr xe
    _ -> mapM evalExpr xe >>= \xv ->
      case fv of
        HiValueFunction ff -> applyFun ff xv
        HiValueString fs   -> applyString fs xv
        HiValueList fl     -> applyList fl xv
        HiValueBytes fb    -> applyBytes fb xv
        HiValueDict fd     -> applyDict fd xv
        _                  -> throwError HiErrorInvalidFunction
evalExpr (HiExprRun ae) = evalExpr ae >>= \case
  HiValueAction aa -> lift $ runAction aa
  _ -> throwInvalidArg
evalExpr (HiExprDict de) = HiValueDict . M.fromList 
                           <$> mapM (\(k, v) -> (,) <$> evalExpr k <*> evalExpr v) de

applyLazyIf, applyLazyAnd, applyLazyOr :: HiMonad m => [HiExpr] -> HiExcept m HiValue
applyLazyIf = ternary $ \x' y' z' -> evalExpr x' >>= \case
  HiValueBool True -> evalExpr y'
  HiValueBool False -> evalExpr z'
  _ -> throwInvalidArg
applyLazyAnd = binary $ \x' y' -> evalExpr x' >>= \x -> if isNullOrFalse x then return x else evalExpr y'
applyLazyOr = binary $ \x' y' -> evalExpr x' >>= \x -> if isNullOrFalse x then evalExpr y' else return x

isNullOrFalse :: HiValue -> Bool
isNullOrFalse = \case
  HiValueNull -> True
  HiValueBool False -> True
  _ -> False

-----------------------------------------------------------------------------------------

applyFun :: HiMonad m => HiFun -> [HiValue] -> HiExcept m HiValue
applyFun HiFunAdd = binary $ \x' y' -> case (x', y') of
  (HiValueNumber x, HiValueNumber y) -> return $ HiValueNumber $ x + y
  (HiValueString x, HiValueString y) -> return $ HiValueString $ x <> y
  (HiValueList x, HiValueList y)     -> return $ HiValueList $ x <> y
  (HiValueBytes x, HiValueBytes y)   -> return $ HiValueBytes $ x <> y
  (HiValueTime x, HiValueNumber y)   -> return $ HiValueTime $ addUTCTime (realToFrac y) x
  _ -> throwInvalidArg
applyFun HiFunSub = binary $ \x' y' -> case (x', y') of
  (HiValueNumber x, HiValueNumber y) -> return $ HiValueNumber $ x - y
  (HiValueTime x, HiValueTime y)     -> return $ HiValueNumber $ toRational $ diffUTCTime x y
  _ -> throwInvalidArg
applyFun HiFunMul = binary $ \x' y' -> case (x', y') of
  (HiValueNumber x, HiValueNumber y) -> return $ HiValueNumber $ x * y
  (HiValueString x, HiValueNumber y) -> repeat' HiValueString x y
  (HiValueList x, HiValueNumber y)   -> repeat' HiValueList x y
  (HiValueBytes x, HiValueNumber y)  -> repeat' HiValueBytes x y
  _ -> throwInvalidArg
  where
    repeat' cons val mult = rationalToInt mult >>= \mr ->
      if mr <= 0 then throwInvalidArg
      else return $ cons $ stimes mr val
applyFun HiFunDiv = binary $ \x' y' -> case (x', y') of
  (HiValueNumber x, HiValueNumber y) ->
    if y == 0 then throwError HiErrorDivideByZero
    else return $ HiValueNumber $ x / y
  (HiValueString x, HiValueString y) -> return $ HiValueString $ x <> "/" <> y
  _ -> throwInvalidArg

applyFun HiFunNot = unary $ \case
  HiValueBool x -> return $ HiValueBool $ not x
  _ -> throwInvalidArg

applyFun HiFunLessThan = binaryCmp (<)
applyFun HiFunGreaterThan = binaryCmp (>)
applyFun HiFunEquals = binaryCmp (==)
applyFun HiFunNotLessThan = binaryCmp (>=)
applyFun HiFunNotGreaterThan = binaryCmp (<=)
applyFun HiFunNotEquals = binaryCmp (/=)

applyFun HiFunLength = unary $ \case
  HiValueString s -> returnInt $ T.length s
  HiValueList l   -> returnInt $ S.length l
  _ -> throwInvalidArg
  where
    returnInt = return . HiValueNumber . toRational
applyFun HiFunToUpper = unaryModifyStr T.toUpper
applyFun HiFunToLower = unaryModifyStr T.toLower
applyFun HiFunReverse = unary $ \case
  HiValueString s -> return $ HiValueString $ T.reverse s
  HiValueList l   -> return $ HiValueList $ S.reverse l
  _ -> throwInvalidArg
applyFun HiFunTrim = unaryModifyStr T.strip
applyFun HiFunList = return . HiValueList . S.fromList
applyFun HiFunRange = binary $ \x' y' -> case (x', y') of
  (HiValueNumber x, HiValueNumber y) -> 
    return $ HiValueList $ S.fromList $ map HiValueNumber [x..y]
  _ -> throwInvalidArg
applyFun HiFunFold = binary $ \x' y' -> case (x', y') of
  (HiValueFunction x, HiValueList (y S.:<| ys)) -> foldM (\a b -> applyFun x [a, b]) y ys
  _ -> throwInvalidArg

applyFun HiFunPackBytes = unary $ \case
  HiValueList l -> fmap (HiValueBytes . B.pack . toList) $ mapM valueToWord8 l
  _ -> throwInvalidArg
  where
    valueToWord8 = \case
      HiValueNumber x -> rationalToInt x >>= \i ->
        if inBounds i then return (fromIntegral i)
        else throwInvalidArg
      _ -> throwInvalidArg
    inBounds z = fromIntegral (minBound :: Word8) <= z && z <= fromIntegral (maxBound :: Word8)

applyFun HiFunUnpackBytes = unary $ \case
  HiValueBytes b -> 
    return $ HiValueList $ S.fromList $ map (HiValueNumber . fromIntegral) $ B.unpack b
  _ -> throwInvalidArg
applyFun HiFunEncodeUtf8 = unary $ \case
  HiValueString s -> return $ HiValueBytes $ encodeUtf8 s
  _ -> throwInvalidArg
applyFun HiFunDecodeUtf8 = unary $ \case
  HiValueBytes b -> return $ either (const HiValueNull) HiValueString $ decodeUtf8' b
  _ -> throwInvalidArg
applyFun HiFunZip = unary $ \case
  HiValueBytes b -> return $ HiValueBytes $ toStrict $ compressWith defaultCompressParams 
                    { compressLevel = bestCompression } $ fromStrict b
  _ -> throwInvalidArg
applyFun HiFunUnzip = unary $ \case
  HiValueBytes b -> return $ HiValueBytes $ toStrict $ decompress $ fromStrict b
  _ -> throwInvalidArg
applyFun HiFunSerialise = unary $ return . HiValueBytes . toStrict . serialise
applyFun HiFunDeserialise = unary $ \case
  HiValueBytes b -> return $ either (const HiValueNull) id $ deserialiseOrFail $ fromStrict b
  _ -> throwInvalidArg

applyFun HiFunRead = unary $ \case
  HiValueString path -> returnFileAsAction HiActionRead path
  _ -> throwInvalidArg
applyFun HiFunWrite = binary $ \x' y' -> case (x', y') of
  (HiValueString path, HiValueString txt) -> 
    returnFileAsAction (flip HiActionWrite $ encodeUtf8 txt) path
  _ -> throwInvalidArg
applyFun HiFunMkDir = unary $ \case
  HiValueString path -> returnFileAsAction HiActionMkDir path
  _ -> throwInvalidArg
applyFun HiFunChDir = unary $ \case
  HiValueString path -> returnFileAsAction HiActionChDir path
  _ -> throwInvalidArg

applyFun HiFunParseTime = unary $ \case
  HiValueString time -> return $ fromMaybe HiValueNull 
                        $ HiValueTime <$> (readMaybe (T.unpack time) :: Maybe UTCTime)
  _ -> throwInvalidArg

applyFun HiFunRand = binary $ \x' y' -> case (x', y') of
  (HiValueNumber x, HiValueNumber y) -> do
    xi <- rationalToInt x
    yi <- rationalToInt y
    return $ HiValueAction $ HiActionRand xi yi
  _ -> throwInvalidArg

applyFun HiFunEcho = unary $ \case
  HiValueString s -> return $ HiValueAction $ HiActionEcho s
  _ -> throwInvalidArg

applyFun HiFunCount = unary $ \case
  HiValueString s -> count T.foldr (HiValueString . T.singleton) s
  HiValueBytes bs -> count B.foldr (HiValueNumber . toRational) bs
  HiValueList l   -> count foldr id l
  _ -> throwInvalidArg
  where
    count fldr keyToHiVal = return . HiValueDict . M.mapKeys keyToHiVal . M.map HiValueNumber 
                            . fldr (flip (M.insertWith (+)) 1) M.empty
applyFun HiFunKeys = unary $ \case
  HiValueDict d -> return $ HiValueList $ S.fromList $ M.keys d
  _ -> throwInvalidArg
applyFun HiFunValues = unary $ \case
  HiValueDict d -> return $ HiValueList $ S.fromList $ M.elems d
  _ -> throwInvalidArg
applyFun HiFunInvert = unary $ \case
  HiValueDict d -> return $ HiValueDict $ M.map HiValueList 
                   $ M.foldrWithKey (\k v -> M.insertWith (S.><) v $ S.singleton k) M.empty d
  _ -> throwInvalidArg

applyFun _ = const throwInvalidArg


throwInvalidArg :: HiMonad m => HiExcept m a
throwInvalidArg = throwError HiErrorInvalidArgument

unary :: HiMonad m => (a -> HiExcept m HiValue) -> [a] -> HiExcept m HiValue
unary f [x] = f x
unary _ _   = throwError HiErrorArityMismatch

binary :: HiMonad m => (a -> a -> HiExcept m HiValue) -> [a] -> HiExcept m HiValue
binary f [x, y] = f x y
binary _ _      = throwError HiErrorArityMismatch

ternary :: HiMonad m => (a -> a -> a -> HiExcept m HiValue) -> [a] -> HiExcept m HiValue
ternary f [x, y, z] = f x y z
ternary _ _         = throwError HiErrorArityMismatch


binaryCmp :: HiMonad m => (t -> t -> Bool) -> [t] -> HiExcept m HiValue
binaryCmp f = binary $ \x y -> return $ HiValueBool $ f x y

unaryModifyStr :: HiMonad m => (T.Text -> T.Text) -> [HiValue] -> HiExcept m HiValue
unaryModifyStr f = unary $ \case
  HiValueString s -> return $ HiValueString $ f s
  _ -> throwInvalidArg

returnFileAsAction :: HiMonad m => (FilePath -> HiAction) -> T.Text -> HiExcept m HiValue
returnFileAsAction action filePath = return $ HiValueAction $ action $ T.unpack filePath

-----------------------------------------------------------------------------------------

applyString :: HiMonad m => T.Text -> [HiValue] -> HiExcept m HiValue
applyString s = applyIndexed (HiValueString . T.singleton . (T.index s)) 
                             (\i j -> HiValueString $ T.drop i $ T.take j s) 
                             (T.length s)

applyList :: HiMonad m => S.Seq HiValue -> [HiValue] -> HiExcept m HiValue
applyList l = applyIndexed (S.index l) 
                           (\i j -> HiValueList $ S.drop i $ S.take j l) 
                           (S.length l)

applyBytes :: HiMonad m => B.ByteString -> [HiValue] -> HiExcept m HiValue
applyBytes b = applyIndexed (HiValueNumber . toRational . (B.index b)) 
                            (\i j -> HiValueBytes $ B.drop i $ B.take j b) 
                            (B.length b)


applyIndexed 
  :: HiMonad m 
  => (Int -> HiValue) 
  -> (Int -> Int -> HiValue) 
  -> Int 
  -> [HiValue] 
  -> HiExcept m HiValue
applyIndexed at slice len = \case
  [(HiValueNumber i')] -> rationalToInt i' >>= \i ->
    return $ if i < 0 || i >= len then HiValueNull else at i
  [i', j'] -> liftA2 slice (indexOrDefault i' 0 len) (indexOrDefault j' len len)
  _ -> throwError HiErrorArityMismatch
  where
    indexOrDefault (HiValueNumber x') _ y' = rationalToInt x' >>= \x -> 
      return $ if x >= 0 then x else (x + y')
    indexOrDefault HiValueNull def _ = return def
    indexOrDefault _ _ _ = throwInvalidArg


applyDict :: HiMonad m => M.Map HiValue HiValue -> [HiValue] -> HiExcept m HiValue
applyDict mp = unary $ return . fromMaybe HiValueNull . (M.!?) mp

rationalToInt :: HiMonad m => Rational -> HiExcept m Int
rationalToInt x
  | xd == 1 && xInBounds = return $ fromInteger $ xn
  | otherwise = throwInvalidArg
  where
    xn = numerator x
    xd = denominator x
    xInBounds = fromIntegral (minBound :: Int) <= xn && xn <= fromIntegral (maxBound :: Int)
