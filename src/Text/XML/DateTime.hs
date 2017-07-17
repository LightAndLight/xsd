{-# language RecordWildCards, OverloadedStrings, TemplateHaskell #-}

module Text.XML.DateTime
  ( DateTime
  , isDateTime
  , mkDateTime
  , parseDateTime
  , dt
  , _DateTime
  )
  where

import Prelude

import Control.Lens (Prism', prism', review)
import Data.Attoparsec.Text (parseOnly)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Text.Parser.Char

import Text.XML.Date
import Text.XML.Time

data DateTime
  = DateTime
  { _dtDate :: Date
  , _dtTime :: Time
  }

isDateTime :: Text -> Bool
isDateTime = isJust . mkDateTime

mkDateTime :: Text -> Maybe DateTime
mkDateTime i =
  case parseOnly parseDateTime i of
    Right d -> Just d
    _ -> Nothing

parseDateTime :: CharParsing m => m DateTime
parseDateTime = DateTime <$> parseDate <* char 'T' <*> parseTime

_DateTime :: Prism' Text DateTime
_DateTime =
  prism'
  (\DateTime{..} -> review _Date _dtDate <> "T" <> review _Time _dtTime)
  mkDateTime
