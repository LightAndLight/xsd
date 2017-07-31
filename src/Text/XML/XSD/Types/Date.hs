{-|
Module: Text.XML.XSD.Types.Date
Description: XSD @date@ type

The @boolean@ type.

https://www.w3.org/TR/xmlschema-2/#date
-}

{-#
language

DeriveLift, RecordWildCards, TemplateHaskell
#-}

module Text.XML.XSD.Types.Date
  ( Date
  , isDate
  , mkDate
  , parseDate
  , date
  , _Date
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

data Era = BCE | CE deriving Lift

data Date
  = Date
  { _dtEra :: Era
  , _dtYears :: Int
  , _dtMonths :: Int
  , _dtDays :: Int
  }
  deriving Lift

isDate :: Text -> Bool
isDate = isJust . mkDate

mkDate :: Text -> Maybe Date
mkDate i =
  case parseOnly parseDate i of
    Right d -> Just d
    _ -> Nothing

parseDate :: CharParsing m => m Date
parseDate =
  Date <$>
  (try (char '-' $> BCE) <|> pure CE) <*>
  (read <$> liftA2 (<>) (replicateM 4 digit) (many digit)) <*
  char '-' <*>
  (read <$> replicateM 2 digit) <*
  char '-' <*>
  (read <$> replicateM 2 digit) <*
  eof

date :: QuasiQuoter
date =
  QuasiQuoter
  { quoteExp = \str ->
      case mkDate (T.pack str) of
        Just d -> [| d |]
        Nothing -> fail $ str <> " is not a valid Date"
  , quotePat = error "`date` cannot be used as a pattern"
  , quoteType = error "`date` cannot be used as a type"
  , quoteDec = error "`date` cannot be used as a declaration"
  }

_Date :: Prism' Text Date
_Date =
  prism'
  (\Date{..} -> T.pack $
    padLeft 4 '0' (show _dtYears) <> "-" <>
    padLeft 2 '0' (show _dtMonths) <> "-" <>
    padLeft 2 '0' (show _dtDays)
  )
  mkDate
  where
    padLeft n c s
      | ls <- length s
      , ls < n = replicate (n - ls) c <> s
      | otherwise = s
