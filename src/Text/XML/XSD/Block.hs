{-#
language

LambdaCase, MultiParamTypeClasses, OverloadedStrings
#-}

module Text.XML.XSD.Block where

import Prelude

import Control.Lens
import Data.Text (Text)

import Text.XML.XSD.Types

class AsBlock s a where
  _Block :: Prism' s a

instance AsBlock Text CTBlock where
  _Block =
    prism'
      (\case
        CTBExtension -> "extension"
        CTBRestriction -> "restriction")

      (\case
        "extension" -> Just CTBExtension
        "restriction" -> Just CTBRestriction
        _ -> Nothing)
