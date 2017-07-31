{-|
Module: Text.XML.XSD.Types.Double
Description: XSD @double@ type

The @double@ type.

https://www.w3.org/TR/xmlschema-2/#double
-}

{-# language DeriveLift, OverloadedStrings, TemplateHaskell #-}

module Text.XML.XSD.Types.Double
  ( isDouble
  , mkDouble
  , parseDouble
  , db
  , _Double
  )
  where

import Prelude

import Control.Applicative ((<|>))
import Control.Lens (Prism', prism', view, re)
import Data.Attoparsec.Text (parseOnly)
import Data.Maybe (isJust)
import Data.Monoid
import Data.Text (Text)
import Language.Haskell.TH.Quote
import Text.Parser.Char
import Text.Parser.Combinators

import qualified Data.Text as T

import Text.XML.XSD.Types.Decimal
import Text.XML.XSD.Types.Numbers.Sign

isDouble :: Text -> Bool
isDouble = isJust . mkDouble

mkDouble :: Text -> Maybe Double
mkDouble a =
  case parseOnly parseDouble a of
    Right f -> Just f
    _ -> Nothing

data Double' = Double' Sign Decimal Sign Text

toDouble :: Double' -> Double
toDouble (Double' s1 m s2 e) =
  read .
  T.unpack $
  showSign s1 <>
  view (re _Decimal) m <>
  "e" <>
  showSign s2 <>
  e
    
-- Again, roundabout
parseDouble :: CharParsing m => m Double
parseDouble =
  fmap toDouble $ Double' <$>
  sign <*>
  parseDecimal <* (try (char 'e') <|> char 'E') <*>
  sign <*>
  (T.pack <$> some digit) <* eof

db :: QuasiQuoter
db =
  QuasiQuoter
  { quoteExp = \str ->
      case mkDouble (T.pack str) of
        Just f -> [| f |]
        Nothing -> fail $ str <> " is not a valid Double"
  , quotePat = error "`db` cannot be used as a pattern"
  , quoteType = error "`db` cannot be used as a type"
  , quoteDec = error "`db` cannot be used as a declaration"
  }

_Double :: Prism' Text Double
_Double =
  prism' (T.pack . show) mkDouble
