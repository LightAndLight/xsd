{-# language DeriveLift #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}

module Text.XML.XSD.Types.Float
  ( Float
  , isFloat
  , mkFloat
  , parseFloat
  , fl
  , _Float
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

isFloat :: Text -> Bool
isFloat = isJust . mkFloat

mkFloat :: Text -> Maybe Float
mkFloat a =
  case parseOnly parseFloat a of
    Right f -> Just f
    _ -> Nothing

data Float' = Float' Sign Decimal Sign Text

toFloat :: Float' -> Float
toFloat (Float' s1 m s2 e) =
  read .
  T.unpack $
  showSign s1 <>
  view (re _Decimal) m <>
  "e" <>
  showSign s2 <>
  e
    
-- This is terribly roundabout, but I want proper parsing support for all
-- types
parseFloat :: CharParsing m => m Float
parseFloat =
  fmap toFloat $ Float' <$>
  sign <*>
  parseDecimal <* (try (char 'e') <|> char 'E') <*>
  sign <*>
  (T.pack <$> some digit) <* eof

fl :: QuasiQuoter
fl =
  QuasiQuoter
  { quoteExp = \str ->
      case mkFloat (T.pack str) of
        Just f -> [| f |]
        Nothing -> fail $ str <> " is not a valid Float"
  , quotePat = error "`fl` cannot be used as a pattern"
  , quoteType = error "`fl` cannot be used as a type"
  , quoteDec = error "`fl` cannot be used as a declaration"
  }

_Float :: Prism' Text Float
_Float =
  prism' (T.pack . show) mkFloat
