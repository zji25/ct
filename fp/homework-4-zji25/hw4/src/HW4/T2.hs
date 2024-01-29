{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , pDigits
  , pSpace
  , pCharIS
  , pCharEqualTo
  , pCharEqualToIS
  , pEof
  , pDouble
  , parseError
  , parseExpr
  ) where

import           Numeric.Natural     (Natural)
import           Control.Applicative
import           Control.Monad
import           Data.Char           (isDigit, digitToInt, isSpace)
import           Data.Maybe          (fromMaybe)
import           Data.List.NonEmpty  (NonEmpty(..))
import           GHC.Float           (int2Double)

import           HW4.Types
import           HW4.T1              (ExceptState(..))


data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

-- helper function to shorten extracting es process, 
-- not really neccessary ig but looks pretty
parse :: Parser a -> (Natural, String) -> Except ParseError (Annotated (Natural, String) a)
parse (P (ES es)) = es  

runP :: Parser a -> String -> Except ParseError a
runP parser = mapExcept (\(a :# _) -> a) . parse parser . (0, )

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error $ ErrorAtPos pos
    (c:cs) -> Success $ c :# (pos + 1, cs)

pSpace :: Parser [Char]
pSpace = many $ mfilter isSpace pChar

-- "IS" stands for "ignoring space", parses first non-space char
-- used everywhere except for parsing numbers
pCharIS :: Parser Char 
pCharIS = pSpace *> pChar

pCharEqualTo :: Char -> Parser Char
pCharEqualTo char = mfilter (== char) pChar

pCharEqualToIS :: Char -> Parser Char 
pCharEqualToIS char = mfilter (== char) pCharIS

parseError :: Parser a
parseError = P $ ES $ Error . ErrorAtPos . fst

instance Alternative Parser where
  empty = parseError
  p1 <|> p2 = P $ ES $ \x -> 
    case parse p1 x of 
      Success sc -> Success sc 
      _          -> parse p2 x

-- No methods
instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  case s of
    [] -> Success $ () :# (pos, [])
    _  -> Error $ ErrorAtPos pos

pDigits :: Parser [Int]
pDigits = some $ digitToInt <$> mfilter isDigit pChar

pDouble :: Parser Double
pDouble = do 
  _        <- pSpace
  sgn      <- maybe 1 (const $ -1) <$> optional (pCharEqualTo '-')
  intPart  <- foldl (\acc x -> acc * 10 + x) 0 <$> pDigits
  fracPart <- optional $ do
    _    <- pCharEqualTo '.'
    foldr (\x acc -> (acc + fromIntegral x) / 10) 0 <$> pDigits
  return $ (int2Double intPart + fromMaybe 0 fracPart) * sgn

pVal :: Parser Expr 
pVal = Val <$> pDouble


{-
  this grammar for parsing arithmetic expressions was generously 
  gifted by translation methods lab
  rules after left recursion elimination:
    E  -> TE'
    E' -> +TE' | -TE' | eof
    T  -> FT'
    T' -> *FT' | /FT' | eof
    F  -> n | (E)
-}

pCommon :: Parser Expr -> Parser (Expr -> Expr -> Expr, Expr) -> Parser Expr
pCommon pFirst pRest = do 
  first <- pFirst
  rest  <- many pRest 
  return $ foldl (\x (op, y) -> x `op` y) first rest 

-- nonempty is required due to the use of foldr1
pCommon' :: NonEmpty Char -> Parser Expr -> Parser (Expr -> Expr -> Expr, Expr)
pCommon' ops pNext = do
  op   <- foldr1 (<|>) $ pCharEqualToIS <$> ops
  next <- pNext
  return (charToFunction op, next)

pE :: Parser Expr
pE = pCommon pT pE' 

pE' :: Parser (Expr -> Expr -> Expr, Expr)
pE' = pCommon' ('+':|['-']) pT

pT :: Parser Expr 
pT = pCommon pF pT'

pT' :: Parser (Expr -> Expr -> Expr, Expr)
pT' = pCommon' ('*':|['/']) pF

pF :: Parser Expr 
pF = pVal <|> pCharEqualToIS '(' *> pE <* pCharEqualToIS ')'

charToFunction :: Char -> Expr -> Expr -> Expr
charToFunction '+' = (+)
charToFunction '-' = (-)
charToFunction '*' = (*)
charToFunction '/' = (/)
charToFunction x   = error $ "unknown operation: \"" ++ x : "\" [should not go here]"

parseExpr :: String -> Except ParseError Expr
parseExpr = runP $ pE <* pEof
