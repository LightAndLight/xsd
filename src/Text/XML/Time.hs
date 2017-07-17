{-# language DeriveLift #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}

module Text.XML.Time
  ( Time
  , isTime
  , mkTime
  , parseTime
  , tm
  , _Time
  )
  where

import Prelude

import Control.Applicative
import Control.Monad
import Control.Lens (Prism', prism')
import Data.Attoparsec.Text (parseOnly)
import Data.Functor
import Data.Maybe (isJust)
import Data.Monoid
import Data.Text (Text)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Text.Parser.Char
import Text.Parser.Combinators

import qualified Data.Text as T

data Sign = Plus | Minus
  deriving Lift

showSign :: Sign -> String
showSign Plus = "+"
showSign Minus = "-"

data TimeZone
  = TimeZone
  { _tzSign :: Sign
  , _tzHours :: Int
  , _tzMinutes :: Int
  }
  deriving Lift

data Time
  = Time
  { _tmHour :: Int
  , _tmMinute :: Int
  , _tmSecond :: Int
  , _tmFraction :: Maybe Integer
  , _tmTimeZone :: Maybe TimeZone
  }
  deriving Lift

isTime :: Text -> Bool
isTime = isJust . mkTime

mkTime :: Text -> Maybe Time
mkTime i =
  case parseOnly parseTime i of
    Right t -> Just t
    _ -> Nothing

parseField :: (CharParsing m, Applicative m) => m Int
parseField = read <$> replicateM 2 digit
    
parseTime :: CharParsing m => m Time
parseTime =
  Time <$>
  parseField <*>
  parseField <*>
  parseField <*>
  optional (char '.' *> (read <$> some digit)) <*>
  optional parseTimeZone <*
  eof

parseTimeZone :: CharParsing m => m TimeZone
parseTimeZone =
  TimeZone <$>
  ((try (char '-') $> Minus) <|> (char '+' $> Plus)) <*>
  parseField <*>
  parseField

tm :: QuasiQuoter
tm =
  QuasiQuoter
  { quoteExp = \str ->
      case mkTime (T.pack str) of
        Just t -> [| t |]
        Nothing -> fail $ str <> " is not a valid Time"
  , quotePat = error "`tm` cannot be used as a pattern"
  , quoteType = error "`tm` cannot be used as a type"
  , quoteDec = error "`tm` cannot be used as a declaration"
  }

_Time :: Prism' Text Time
_Time =
  prism'
  (\Time{..} -> T.pack $
     show _tmHour <> ":" <>
     show _tmMinute <> ":" <>
     show _tmSecond <>
     maybe "" ((<>) "." . show) _tmFraction <>
     maybe ""
       (\TimeZone{..} ->
          showSign _tzSign <>
          show _tzHours <>
          show _tzMinutes)
       _tmTimeZone
  )
  mkTime
