{-# language TemplateHaskell #-}

module Text.XML.XSD.Types.NonNegative
  ( NonNegative
  , isNonNegative
  , mkNonNegative
  , nn
  , _NonNegative
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

type NonNegative = Natural

isNonNegative :: Text -> Bool
isNonNegative = isJust . mkNonNegative

mkNonNegative :: Text -> Maybe NonNegative
mkNonNegative a =
  case parseOnly parseNonNegative a of
    Right n -> Just n
    _ -> Nothing

parseNonNegative :: CharParsing m => m NonNegative
parseNonNegative = read <$> some digit

nn :: QuasiQuoter
nn =
  QuasiQuoter
  { quoteExp = \str ->
      case mkNonNegative (T.pack str) of
        Just n -> [| n |]
        Nothing -> fail $ str <> " is not a valid NonNegative"
  , quotePat = error "`nn` cannot be used as a pattern"
  , quoteType = error "`nn` cannot be used as a type"
  , quoteDec = error "`nn` cannot be used as a declaration"
  }

_NonNegative :: Prism' Text NonNegative
_NonNegative = prism' (T.pack . show) mkNonNegative
