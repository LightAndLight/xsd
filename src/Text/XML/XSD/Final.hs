{-#
language

LambdaCase, MultiParamTypeClasses, OverloadedStrings
#-}

module Text.XML.XSD.Final (AsFinal(..)) where

import Prelude

import Control.Lens
import Data.Text (Text)

import qualified Data.Text as T

import Text.XML.XSD.Types

class AsFinal s a where
  _Final :: Prism' s a

instance AsFinal Text CTFinal where
  _Final =
    prism'
      (\case
          CTFExtension -> "extension"
          CTFRestriction -> "restriction")

      (\case
          "extension" -> Just CTFExtension
          "restriction" -> Just CTFRestriction
          _ -> Nothing)

instance AsFinal Text STFinal where
  _Final =
    prism'
      (\case
          STAll -> "#all"
          STMultiple elems -> T.unwords $ fmap showSTFFinal elems)

      (\case
          "#all" -> Just STAll
          i -> STMultiple <$> traverse parseSTFFinal (T.words i))
    where
      parseSTFFinal =
        \case
          "list" -> Just STFList
          "union" -> Just STFUnion
          "restriction" -> Just STFRestriction
          _ -> Nothing

      showSTFFinal =
        \case
          STFList -> "list"
          STFUnion -> "union"
          STFRestriction -> "restriction"
