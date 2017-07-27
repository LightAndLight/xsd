{-# language DeriveLift, TemplateHaskell #-}

module Text.XML.XSD.Types.Positive
  ( Positive
  , isPositive
  , mkPositive
  , pos
  , _Positive
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

newtype Positive = Positive Natural
  deriving (Eq, Show, Ord, Lift)

isPositive :: Text -> Bool
isPositive = isJust . mkPositive

mkPositive :: Text -> Maybe Positive
mkPositive a =
  case parseOnly parsePositive a of
    Right n -> Just n
    _ -> Nothing

parsePositive :: CharParsing m => m Positive
parsePositive =
  Positive . read <$> liftA2 (:) (oneOf "123456789") (many digit)

pos :: QuasiQuoter
pos =
  QuasiQuoter
  { quoteExp = \str ->
      case mkPositive (T.pack str) of
        Just n -> [| n |]
        Nothing -> fail $ str <> " is not a valid Positive"
  , quotePat = error "`pos` cannot be used as a pattern"
  , quoteType = error "`pos` cannot be used as a type"
  , quoteDec = error "`pos` cannot be used as a declaration"
  }

_Positive :: Prism' Text Positive
_Positive = prism' (T.pack . show) mkPositive
