{-# language DeriveLift #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}

module Text.XML.Decimal
  ( Decimal
  , isDecimal
  , mkDecimal
  , dc
  , _Decimal
  )
  where

import Prelude

import Control.Lens (Prism', prism')
import Data.Char (isDigit)
import Data.Maybe (isJust)
import Data.Monoid
import Data.Text (Text)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import qualified Data.Text as T

data Sign = Pos | Neg deriving Lift

showSign :: Sign -> Text
showSign a =
  case a of
    Pos -> "+"
    Neg -> "-"

data Decimal
  = Decimal
  { _decSign :: Sign
  , _decWhole :: Text
  , _decFrac :: Text
  }

instance Lift Decimal where
  lift (Decimal s l r) =
    let
      l' = T.unpack l
      r' = T.unpack r
    in [| Decimal s (T.pack l') (T.pack r') |]

isDecimal :: String -> Bool
isDecimal = isJust . mkDecimal

mkDecimal :: String -> Maybe Decimal
mkDecimal "" = Nothing
mkDecimal (x:xs) = _

dc :: QuasiQuoter
dc =
  QuasiQuoter
  { quoteExp = \str ->
      case mkDecimal str of
        Just d -> [| d |]
        Nothing -> fail $ str <> " is not a valid Decimal"
  , quotePat = error "`dc` cannot be used as a pattern"
  , quoteType = error "`dc` cannot be used as a type"
  , quoteDec = error "`dc` cannot be used as a declaration"
  }

_Decimal :: Prism' Text Decimal
_Decimal =
  prism'
  (\(Decimal s l r) -> showSign s <> l <> "." <> r)
  (mkDecimal . T.unpack)
