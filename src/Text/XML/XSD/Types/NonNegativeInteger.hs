{-|
Module: Text.XML.XSD.Types.NonNegativeInteger
Description: XSD @nonNegativeInteger@ type

The @nonNegativeInteger@ type.

https://www.w3.org/TR/xmlschema-2/#nonNegativeInteger
-}

{-# language TemplateHaskell #-}
module Text.XML.XSD.Types.NonNegativeInteger
  ( NonNegativeInteger
  , isNonNegativeInteger
  , mkNonNegativeInteger
  , nn
  , _NonNegativeInteger
  )
  where

import Prelude

import Control.Lens (Prism', prism')
import Data.Attoparsec.Text (parseOnly)
import Data.Maybe (isJust)
import Data.Monoid
import Data.Text (Text)
import Language.Haskell.TH.Quote
import Numeric.Natural
import Text.Parser.Char
import Text.Parser.Combinators

import qualified Data.Text as T

type NonNegativeInteger = Natural

isNonNegativeInteger :: Text -> Bool
isNonNegativeInteger = isJust . mkNonNegativeInteger

mkNonNegativeInteger :: Text -> Maybe NonNegativeInteger
mkNonNegativeInteger a =
  case parseOnly parseNonNegativeInteger a of
    Right n -> Just n
    _ -> Nothing

parseNonNegativeInteger :: CharParsing m => m NonNegativeInteger
parseNonNegativeInteger = read <$> some digit

nn :: QuasiQuoter
nn =
  QuasiQuoter
  { quoteExp = \str ->
      case mkNonNegativeInteger (T.pack str) of
        Just n -> [| n |]
        Nothing -> fail $ str <> " is not a valid NonNegativeInteger"
  , quotePat = error "`nn` cannot be used as a pattern"
  , quoteType = error "`nn` cannot be used as a type"
  , quoteDec = error "`nn` cannot be used as a declaration"
  }

_NonNegativeInteger :: Prism' Text NonNegativeInteger
_NonNegativeInteger = prism' (T.pack . show) mkNonNegativeInteger
