{-|
Module: Text.XML.XSD.XMLRep.Notation
Description: XSD @notation@ element

The XSD @notation@ element

https://www.w3.org/TR/xmlschema-1/#element-notation
-}

module Text.XML.XSD.XMLRep.Notation
  ( Notation(..)
  -- * Lenses
  , AsNotation(..)
  , notID
  , notName
  , notPublic
  , notSystem
  , notAttrs
  )
  where

import Text.XML.XSD.XMLRep.Internal.Lenses
import Text.XML.XSD.XMLRep.Internal.Types
