{-# language DeriveLift #-}
{-# language OverloadedStrings #-}

module Text.XML.XSD.Types.Numbers.Sign where

import Control.Applicative ((<|>), pure)
import Data.Functor (($>))
import Data.Text (Text)
import Language.Haskell.TH.Syntax
import Text.Parser.Char
import Text.Parser.Combinators

data Sign = Pos | Neg deriving Lift

showSign :: Sign -> Text
showSign a =
  case a of
    Pos -> "+"
    Neg -> "-"

sign :: CharParsing m => m Sign
sign = try (char '+' $> Pos) <|> try (char '-' $> Neg) <|> pure Pos
