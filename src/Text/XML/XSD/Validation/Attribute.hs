{-|
Module: Text.XML.XSD.Validation.Attribute
Description: @Attribute Declaration@ schema component

The @Attribute Declaration@ schema component

https://www.w3.org/TR/xmlschema-1/#Attribute_Declaration_details
-}

module Text.XML.XSD.Validation.Attribute
  ( Attribute(..)
  , AttScope(..)
  , AttValueConstraint(..)
  -- * Lenses
  , attName
  , attTargetNamespace
  , attTypeDefinition
  , attScope
  , attValueConstraint
  )
  where

import Text.XML.XSD.Validation.Internal

