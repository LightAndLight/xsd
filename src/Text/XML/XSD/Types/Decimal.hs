{-|
Module: Text.XML.XSD.Types.Decimal
Description: XSD @decimal@ type

The @decimal@ type.

https://www.w3.org/TR/xmlschema-2/#decimal
-}

{-# language DeriveLift, OverloadedStrings, TemplateHaskell #-}

module Text.XML.XSD.Types.Decimal
  ( Decimal
  , isDecimal
  , mkDecimal
  , parseDecimal
  , dc
  , _Decimal
  )
  where

import Prelude

import Control.Applicative ((<|>))
import Control.Lens (Prism', prism')
import Data.Attoparsec.Text (parseOnly)
import Data.Maybe (isJust)
import Data.Monoid
import Data.Text (Text)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Parser.Char
import Text.Parser.Combinators

import qualified Data.Text as T

import Text.XML.XSD.Types.Numbers.Sign

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

isDecimal :: Text -> Bool
isDecimal = isJust . mkDecimal

mkDecimal :: Text -> Maybe Decimal
mkDecimal a =
  case parseOnly parseDecimal a of
    Right d -> Just d
    _ -> Nothing

parseDecimal :: CharParsing m => m Decimal
parseDecimal =
  let
  in Decimal <$>
     sign <*>
     fmap T.pack (some digit) <*>
     (try (char '.' *> fmap T.pack (many digit)) <|> pure "0") <*
     eof

dc :: QuasiQuoter
dc =
  QuasiQuoter
  { quoteExp = \str ->
      case mkDecimal (T.pack str) of
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
  mkDecimal
