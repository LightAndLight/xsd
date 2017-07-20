{-# language DeriveLift #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}

module Text.XML.XSD.Types.Time
  ( Time
  , isTime
  , mkTime
  , parseTime
  , time
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
  = UTC
  | TimeZone Sign Int Int
  deriving Lift

_tzSign :: TimeZone -> Maybe Sign
_tzSign UTC = Nothing
_tzSign (TimeZone s _ _) = Just s

_tzHours :: TimeZone -> Int
_tzHours UTC = 0
_tzHours (TimeZone _ h _) = h

_tzMinutes :: TimeZone -> Int
_tzMinutes UTC = 0
_tzMinutes (TimeZone _ _ m) = m

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
parseTimeZone = try utc <|> nonutc
  where
    utc = char 'Z' $> UTC
    nonutc = 
      TimeZone <$>
      ((try (char '-') $> Minus) <|> (char '+' $> Plus)) <*>
      parseField <*>
      parseField

time :: QuasiQuoter
time =
  QuasiQuoter
  { quoteExp = \str ->
      case mkTime (T.pack str) of
        Just t -> [| t |]
        Nothing -> fail $ str <> " is not a valid Time"
  , quotePat = error "`time` cannot be used as a pattern"
  , quoteType = error "`time` cannot be used as a type"
  , quoteDec = error "`time` cannot be used as a declaration"
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
       (\case
           UTC -> "+00:00"
           TimeZone s h m ->
             showSign s <>
             show h <>
             show m)
       _tmTimeZone
  )
  mkTime
