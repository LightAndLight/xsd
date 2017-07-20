{-# language OverloadedStrings #-}
module Text.XML.XSD.Types.Boolean
  ( isBoolean
  , mkBoolean
  , parseBoolean
  , _Boolean
  )
  where

import Prelude

import Data.Functor (($>))
import Control.Applicative ((<|>))
import Control.Lens (Prism', prism')
import Data.Attoparsec.Text (parseOnly)
import Data.Maybe (isJust)
import Data.Text (Text)
import Text.Parser.Char
import Text.Parser.Combinators

isBoolean :: Text -> Bool
isBoolean = isJust . mkBoolean

mkBoolean :: Text -> Maybe Bool
mkBoolean a =
  case parseOnly parseBoolean a of
    Right b -> Just b
    _ -> Nothing

parseBoolean :: CharParsing m => m Bool
parseBoolean = (try (text "true" $> True) <|> (text "false" $> False)) <* eof

_Boolean :: Prism' Text Bool
_Boolean = prism' (\a -> if a then "true" else "false") mkBoolean
