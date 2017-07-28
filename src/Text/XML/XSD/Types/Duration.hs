{-#
language

DeriveLift, RecordWildCards, OverloadedStrings, TemplateHaskell
#-}

module Text.XML.XSD.Types.Duration
  ( Duration
  , isDuration
  , mkDuration
  , parseDuration
  , dur
  , _Duration
  )
  where

import Prelude

import Control.Lens (Prism', prism')
import Data.Attoparsec.Text (parseOnly)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Parser.Char
import Text.Parser.Combinators

import qualified Data.Text as T

import Text.XML.XSD.Types.Numbers.Sign

data Duration
  = Duration
  { _durSign :: Sign
  , _durYears :: Int
  , _durMonths :: Int
  , _durDays :: Int
  , _durTime :: TimePart
  }
  deriving (Eq, Lift, Show)

data TimePart
  = TimePart
  { _durHours :: Int
  , _durMinutes :: Int
  , _durSeconds :: Int
  , _durFraction :: Int
  }
  deriving (Eq, Lift, Show)

isDuration :: Text -> Bool
isDuration = isJust . mkDuration

mkDuration :: Text -> Maybe Duration
mkDuration i =
  case parseOnly parseDuration i of
    Right d -> Just d
    _ -> Nothing

-- Doesn't model the grammar exactly; allows P1Y2MT
parseDuration :: (Monad m, CharParsing m) => m Duration
parseDuration = do
  s <- option Pos (try $ char '-' *> pure Neg)
  _ <- char 'P'
  years <- pYears
  months <- pMonths
  days <- pDays
  timePart <-
    option (TimePart 0 0 0 0) $ do
      _ <- char 'T'
      hours <- pHours
      minutes <- pMinutes
      seconds <- pSeconds
      pure $ uncurry (TimePart hours minutes) seconds
  eof
  pure $ Duration s years months days timePart
  where
    digitsFollowedByLetter l =
      option 0 $ try ((read <$> some digit) <* char l)
      
    pYears = digitsFollowedByLetter 'Y'
    pMonths = digitsFollowedByLetter 'M'
    pDays = digitsFollowedByLetter 'D'
    pHours = digitsFollowedByLetter 'H'
    pMinutes = digitsFollowedByLetter 'M'
    pSeconds = option (0, 0) $ do
      sec <- read <$> some digit
      frac <- option 0 $ try (char '.' *> (read <$> some digit))
      _ <- char 'S'
      pure (sec, frac)

dur :: QuasiQuoter
dur =
  QuasiQuoter
  { quoteExp = \str ->
      case mkDuration (T.pack str) of
        Just t -> [| t |]
        Nothing -> fail $ str <> " is not a valid Duration"
  , quotePat = error "`dur` cannot be used as a pattern"
  , quoteType = error "`dur` cannot be used as a type"
  , quoteDec = error "`dur` cannot be used as a declaration"
  }

_Duration :: Prism' Text Duration
_Duration =
  prism'
  (\Duration{..} -> T.pack $
     "P" <>
     show _durYears <> "Y" <>
     show _durMonths <> "M" <>
     show _durDays <> "D" <>
     "T" <>
     show (_durHours _durTime) <> "H" <>
     show (_durMinutes _durTime) <> "M" <>
     show (_durSeconds _durTime) <> "." <>
     show (_durFraction _durTime) <> "S")
  mkDuration
