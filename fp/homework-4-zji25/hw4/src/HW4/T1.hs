module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import           HW4.Types
import           Control.Monad       (ap)

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES es) = ES $ mapExcept (mapAnnotated f) . es

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ wrapExcept . (a :#)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES es) = ES $ joinExcept . mapExcept (\((ES es') :# s) -> es' s) . es

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ wrapExcept . (() :#) . f

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = ap

instance Monad (ExceptState e s) where
  x >>= y = joinExceptState $ mapExceptState y x

data EvaluationError = DivideByZero
  deriving Show

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x)        = return x
eval (Op (Abs x))   = evalUnary x Abs (successfulUnary abs)
eval (Op (Sgn x))   = evalUnary x Sgn (successfulUnary signum)
eval (Op (Add x y)) = evalBinary x y Add (successfulBinary (+))
eval (Op (Sub x y)) = evalBinary x y Sub (successfulBinary (-))
eval (Op (Mul x y)) = evalBinary x y Mul (successfulBinary (*))
eval (Op (Div x y)) = evalBinary x y Div (\x' y' -> if y' == 0 then Error DivideByZero else Success $ x' / y')

evalUnary 
  :: Expr 
  -> (Double -> Prim Double) 
  -> (Double -> Except EvaluationError Double) 
  -> ExceptState EvaluationError [Prim Double] Double
evalUnary x cons calc = do 
  x' <- eval x
  case calc x' of
    Success z' -> do 
      modifyExceptState (cons x' :)
      return z'
    Error e' -> do
      throwExceptState e'

evalBinary 
  :: Expr 
  -> Expr 
  -> (Double -> Double -> Prim Double) 
  -> (Double -> Double -> Except EvaluationError Double) 
  -> ExceptState EvaluationError [Prim Double] Double
evalBinary x y cons calc = do 
  x' <- eval x
  y' <- eval y 
  case calc x' y' of
    Success z' -> do 
      modifyExceptState (cons x' y' :)
      return z'
    Error e' -> do
      throwExceptState e'

successfulUnary :: (Double -> Double) -> Double -> Except EvaluationError Double
successfulUnary f = Success . f

successfulBinary :: (Double -> Double -> Double) -> Double -> Double -> Except EvaluationError Double
successfulBinary f = (Success .) . f

