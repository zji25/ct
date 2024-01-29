module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import           HW3.T1
import           GHC.Real        (Ratio(..))
import           GHC.Float       (rationalToDouble)

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f (S g) = S $ mapAnnotated f . g

wrapState :: a -> State s a
wrapState = S . (:#)

joinState :: State s (State s a) -> State s a
joinState (S f) = S $ \s ->
  let (S xa) :# xs = f s
  in xa xs

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# (f s)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  S f <*> S g = S $ \s ->
    let x :# xs = f s
        y :# ys = g xs
    in (x y) :# ys


instance Monad (State s) where
  return = wrapState
  x >>= y = joinState $ mapState y x


data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  x + y       = Op $ Add x y
  x - y       = Op $ Sub x y
  x * y       = Op $ Mul x y
  abs         = Op . Abs
  signum      = Op . Sgn
  fromInteger = Val . fromInteger

instance Fractional Expr where
  x / y                 = Op $ Div x y
  fromRational (x :% y) = Val $ rationalToDouble x y


eval :: Expr -> State [Prim Double] Double
eval (Val x)        = return x
eval (Op (Add x y)) = evalBinary x y (+) Add
eval (Op (Sub x y)) = evalBinary x y (-) Sub
eval (Op (Mul x y)) = evalBinary x y (*) Mul
eval (Op (Div x y)) = evalBinary x y (/) Div
eval (Op (Abs x))   = evalUnary x abs Abs
eval (Op (Sgn x))   = evalUnary x signum Sgn

evalBinary 
  :: Expr 
  -> Expr 
  -> (Double -> Double -> Double) 
  -> (Double -> Double -> Prim Double) 
  -> State [Prim Double] Double
evalBinary x y calc cons = do 
  xv <- eval x
  yv <- eval y 
  modifyState (cons xv yv :)
  return (calc xv yv)

evalUnary 
  :: Expr 
  -> (Double -> Double) 
  -> (Double -> Prim Double) 
  -> State [Prim Double] Double
evalUnary x calc cons = do 
  xv <- eval x
  modifyState (cons xv :)
  return (calc xv)