{-|
Module: Text.XML.XSD.XMLRep.Block
Description: XSD @block@ attribute

Prism for the @block@ attribute, used in:

* https://www.w3.org/TR/xmlschema-1/#element-complexType
* https://www.w3.org/TR/xmlschema-1/#element-element
-}

{-#
language

LambdaCase, MultiParamTypeClasses, OverloadedStrings
#-}
module Text.XML.XSD.XMLRep.Fields.Block where

import Prelude

import Control.Lens
import Data.Text (Text)

import Text.XML.XSD.XMLRep.Internal.Types

-- | Prism for @block@-like things
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
