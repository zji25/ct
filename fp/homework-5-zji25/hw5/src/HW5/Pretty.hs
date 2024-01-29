{-# LANGUAGE OverloadedStrings #-}

module HW5.Pretty (prettyValue, prettyError) where

import Data.Char (toLower)
import qualified Data.Foldable as F (toList)
import qualified Data.Map as M (toList)
import Data.Ratio (denominator, numerator)
import Data.Scientific (fromRationalRepetendUnlimited, toRealFloat)
import HW5.Base (HiError, HiShow (..), HiValue (..))
import Prettyprinter (Doc, annotate, encloseSep, pretty, viaShow, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), bold, colorDull)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber n) = clr Cyan $ prettyRational n
prettyValue (HiValueBool b) = clrB Magenta $ pretty $ map toLower $ show b
prettyValue (HiValueFunction f) = pretty $ show f
prettyValue HiValueNull = clrB Magenta "null"
prettyValue (HiValueString s) = clr Green $ viaShow s
prettyValue (HiValueList l) = encloseSep "[" "]" ", " $ map prettyValue $ F.toList l
prettyValue (HiValueBytes b) = pretty $ hiShow b
prettyValue (HiValueAction a) = pretty $ show a
prettyValue (HiValueTime t) = clr Yellow $ viaShow t
prettyValue (HiValueDict d) = encloseSep "{" "}" ", " 
                              $ map (\(k, v) -> prettyValue k <> ":" <+> prettyValue v) 
                              $ M.toList d


prettyRational :: Rational -> Doc AnsiStyle
prettyRational x
  | xd == 1 = pretty xn
  | otherwise = case fromRationalRepetendUnlimited x of
      (sc, Nothing) -> pretty $ (toRealFloat sc :: Double)
      _ -> case q of
        0 -> (if r > 0 then "" else "-") <> frac
        _ -> pretty q <+> (if r > 0 then "+" else "-") <+> frac
  where
    xn = numerator x
    xd = denominator x
    (q, r) = quotRem xn xd
    frac = pretty (abs r) <> "/" <> pretty xd

prettyError :: HiError -> Doc AnsiStyle
prettyError e = clrB Red $ pretty $ show e


clr, clrB :: Color -> Doc AnsiStyle -> Doc AnsiStyle
clr = annotate . colorDull
clrB = annotate . (bold <>) . colorDull
