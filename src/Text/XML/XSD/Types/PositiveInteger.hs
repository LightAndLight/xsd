{-|
Module: Text.XML.XSD.Types.PositiveInteger
Description: XSD @positiveInteger@ type

The @boolean@ type.

https://www.w3.org/TR/xmlschema-2/#positiveInteger
-}

{-# language DeriveLift, TemplateHaskell #-}
module Text.XML.XSD.Types.PositiveInteger
  ( PositiveInteger
  , isPositiveInteger
  , mkPositiveInteger
  , pos
  , _PositiveInteger
  )
  where

import Prelude

import Control.Applicative (liftA2)
import Control.Lens (Prism', prism')
import Data.Attoparsec.Text (parseOnly)
import Data.Maybe (isJust)
import Data.Monoid
import Data.Text (Text)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Lift)
import Numeric.Natural
import Text.Parser.Char
import Text.Parser.Combinators

import qualified Data.Text as T

newtype PositiveInteger = PositiveInteger Natural
  deriving (Eq, Show, Ord, Lift)

isPositiveInteger :: Text -> Bool
isPositiveInteger = isJust . mkPositiveInteger

mkPositiveInteger :: Text -> Maybe PositiveInteger
mkPositiveInteger a =
  case parseOnly parsePositiveInteger a of
    Right n -> Just n
    _ -> Nothing

parsePositiveInteger :: CharParsing m => m PositiveInteger
parsePositiveInteger =
  PositiveInteger . read <$> liftA2 (:) (oneOf "123456789") (many digit)

pos :: QuasiQuoter
pos =
  QuasiQuoter
  { quoteExp = \str ->
      case mkPositiveInteger (T.pack str) of
        Just n -> [| n |]
        Nothing -> fail $ str <> " is not a valid PositiveInteger"
  , quotePat = error "`pos` cannot be used as a pattern"
  , quoteType = error "`pos` cannot be used as a type"
  , quoteDec = error "`pos` cannot be used as a declaration"
  }

_PositiveInteger :: Prism' Text PositiveInteger
_PositiveInteger = prism' (T.pack . show) mkPositiveInteger
