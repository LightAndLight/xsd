{-# language DeriveLift, RecordWildCards, OverloadedStrings, TemplateHaskell #-}

module Text.XML.XSD.Types.DateTime
  ( DateTime
  , isDateTime
  , mkDateTime
  , parseDateTime
  , datetime
  , _DateTime
  )
  where

import Prelude

import Control.Lens (Prism', prism', review)
import Data.Attoparsec.Text (parseOnly)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Parser.Char

import qualified Data.Text as T

import Text.XML.XSD.Types.Date
import Text.XML.XSD.Types.Time

data DateTime
  = DateTime
  { _dtDate :: Date
  , _dtTime :: Time
  }
  deriving Lift

isDateTime :: Text -> Bool
isDateTime = isJust . mkDateTime

mkDateTime :: Text -> Maybe DateTime
mkDateTime i =
  case parseOnly parseDateTime i of
    Right d -> Just d
    _ -> Nothing

parseDateTime :: CharParsing m => m DateTime
parseDateTime = DateTime <$> parseDate <* char 'T' <*> parseTime

datetime :: QuasiQuoter
datetime =
  QuasiQuoter
  { quoteExp = \str ->
      case mkDateTime (T.pack str) of
        Just t -> [| t |]
        Nothing -> fail $ str <> " is not a valid DateTime"
  , quotePat = error "`datetime` cannot be used as a pattern"
  , quoteType = error "`datetime` cannot be used as a type"
  , quoteDec = error "`datetime` cannot be used as a declaration"
  }

_DateTime :: Prism' Text DateTime
_DateTime =
  prism'
  (\DateTime{..} -> review _Date _dtDate <> "T" <> review _Time _dtTime)
  mkDateTime
