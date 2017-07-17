{-# language TemplateHaskell #-}

module Text.XML.HexBinary
  ( HexBinary
  , isHexBinary
  , mkHexBinary
  , parseHexBinary
  , hx
  , _HexBinary
  )
  where

import Prelude

import Control.Applicative
import Control.Lens (Prism', prism')
import Data.Attoparsec.Text (parseOnly)
import Data.ByteString (ByteString)
import Data.Functor
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Word
import Language.Haskell.TH.Quote
import Text.Parser.Char
import Text.Parser.Combinators

import qualified Data.ByteString as B
import qualified Data.Text as T

type HexBinary = ByteString

isHexBinary :: Text -> Bool
isHexBinary = isJust . mkHexBinary

mkHexBinary :: Text -> Maybe HexBinary
mkHexBinary i =
  case parseOnly parseHexBinary i of
    Right h -> Just h
    _ -> Nothing

parseHexBinary :: CharParsing m => m ByteString
parseHexBinary = B.pack <$> some byte
  where
    byte = liftA2 (+) ((*16) <$> nibble) nibble
    
    nibble :: CharParsing m => m Word8
    nibble =
      try (char '0' $> 0) <|>
      try (char '1' $> 1) <|>
      try (char '2' $> 2) <|>
      try (char '3' $> 3) <|>
      try (char '4' $> 4) <|>
      try (char '5' $> 5) <|>
      try (char '6' $> 6) <|>
      try (char '7' $> 7) <|>
      try (char '8' $> 8) <|>
      try (char '9' $> 9) <|>
      try (char 'A' $> 10) <|>
      try (char 'B' $> 11) <|>
      try (char 'C' $> 12) <|>
      try (char 'D' $> 13) <|>
      try (char 'E' $> 14) <|>
      (char 'F' $> 15)

hx :: QuasiQuoter
hx =
  QuasiQuoter
  { quoteExp = \str ->
      case mkHexBinary (T.pack str) of
        Just h -> let h' = B.unpack h in [| B.pack h' |]
        Nothing -> fail $ str <> " is not a valid HexBinary"
  , quotePat = error "`hx` cannot be used as a pattern"
  , quoteType = error "`hx` cannot be used as a type"
  , quoteDec = error "`hx` cannot be used as a declaration"
  }

_HexBinary :: Prism' Text HexBinary
_HexBinary = prism' (foldMap wordToText . B.unpack) mkHexBinary
  where
    wordToText :: Word8 -> Text
    wordToText w = T.pack $ fmap nibbleToText [w `div` 16, w `rem` 16]

    nibbleToText :: Word8 -> Char
    nibbleToText n =
      case n of
        0 -> '0'
        1 -> '1'
        2 -> '2'
        3 -> '3'
        4 -> '4'
        5 -> '5'
        6 -> '6'
        7 -> '7'
        8 -> '8'
        9 -> '9'
        10 -> 'A'
        11 -> 'B'
        12 -> 'C'
        13 -> 'D'
        14 -> 'E'
        15 -> 'F'
        _ -> error $
          "_HexBinary: nibbleToText: unexpected input '" <>
          show n <> "'"
