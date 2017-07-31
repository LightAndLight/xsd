{-|
Module: Text.XML.XSD.Types.ID
Description: XSD @ID@ type

The @ID@ type.

https://www.w3.org/TR/xmlschema-2/#ID
-}

module Text.XML.XSD.Types.ID where

import Prelude (Maybe)
import Control.Lens (Lens')

import Text.XML.XSD.Types.NCName

type ID = NCName

class HasID s where
  id' :: Lens' s (Maybe ID)
