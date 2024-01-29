module HW5.Parser (parse) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Data.ByteString as B (pack)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (pack)
import Data.Void (Void)
import HW5.Base
import Numeric (readHex)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char (alphaNumChar, char, hexDigitChar, letterChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as L

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space *> pHiExpr <* eof) ""

type Parser = Parsec Void String

pHiExpr :: Parser HiExpr
pHiExpr = makeExprParser pHiExprApply operatorTable

pHiExprApply :: Parser HiExpr
pHiExprApply = do
  f <- pHiExprValue
  args <- many $ pArgsInBrackets <|> pDotArg
  let baseExpr = foldl HiExprApply f args
  runExpr <- optional $ HiExprRun baseExpr <$ charL '!'
  return $ fromMaybe baseExpr runExpr
  where
    pArgsInBrackets = parensR $ pHiExpr `sepBy` charL ','
    pDotArg = (: []) . HiExprValue . HiValueString . T.pack <$> (char '.' *> pIdent)
    pIdent = intercalate "-" <$> ((:) <$> letterChar <*> many alphaNumChar) `sepBy1` char '-'



pHiExprValue :: Parser HiExpr
pHiExprValue = HiExprValue <$> pHiValue <|> pHiExprList <|> pHiExprDict <|> parensR pHiExpr

pHiValueFunction, pHiValueNumber, pHiValueBool, pHiValueNull, pHiValueString, pHiValueBytes, pHiValueAction :: Parser HiValue
pHiValueFunction = HiValueFunction <$> choice (map (\(cons, name) -> cons <$ stringL name) hiFunNames)
pHiValueNumber = lexeme $ HiValueNumber <$> toRational <$> L.signed space L.scientific
pHiValueBool = HiValueBool <$> (True <$ stringL "true" <|> False <$ stringL "false")
pHiValueNull = HiValueNull <$ stringL "null"
pHiValueString = lexeme $ HiValueString . T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))
pHiValueBytes = HiValueBytes . B.pack <$> (parensB $ pByte `sepBy` space)
  where
    pByte = lexeme $ do
      x <- count 2 hexDigitChar
      case readHex x of
        [(hex, "")] -> return $ fromInteger hex
        _           -> fail "fail"
pHiValueAction = HiValueAction <$> (HiActionCwd <$ stringL "cwd" <|> HiActionNow <$ stringL "now")

pHiValue :: Parser HiValue
pHiValue = pHiValueNumber <|> pHiValueNull <|> pHiValueAction <|> pHiValueBool <|> pHiValueFunction <|> pHiValueString <|> pHiValueBytes

pHiExprList, pHiExprDict :: Parser HiExpr
pHiExprList = HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> (parensS $ pHiExpr `sepBy` charL ',')
pHiExprDict = HiExprDict <$> (parensC $ ((,) <$> pHiExpr <* charL ':' <*> pHiExpr) `sepBy` charL ',')

operatorTable :: [[Operator Parser HiExpr]]
operatorTable = [
  [ binaryL "*" HiFunMul, binaryLNF "/" "=" HiFunDiv]
  , [ binaryL "+" HiFunAdd, binaryL "-" HiFunSub ]
  , [ binaryN "<=" HiFunNotGreaterThan
    , binaryN ">=" HiFunNotLessThan
    , binaryN "<" HiFunLessThan
    , binaryN ">" HiFunGreaterThan
    , binaryN "==" HiFunEquals
    , binaryN "/=" HiFunNotEquals ]
  , [ binaryR "&&" HiFunAnd ]
  , [ binaryR "||" HiFunOr ]]
  where
    binaryToApply f x y = HiExprApply (HiExprValue $ HiValueFunction f) [x, y]
    binaryLNF s1 s2 f = InfixL $ (binaryToApply f) <$ try (stringL s1 <* notFollowedBy (stringL s2))
    binary op s f = op $ (binaryToApply f) <$ stringL s
    binaryL = binary InfixL
    binaryN = binary InfixN
    binaryR = binary InfixR


lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

charL :: Char -> Parser Char
charL c = lexeme $ char c

stringL :: String -> Parser String
stringL s = lexeme $ string s

parens :: String -> String -> Parser a -> Parser a
parens l r = between (stringL l) (stringL r)

parensR, parensS, parensB, parensC :: Parser a -> Parser a
parensR = parens "(" ")"
parensS = parens "[" "]"
parensB = parens "[#" "#]"
parensC = parens "{" "}"
