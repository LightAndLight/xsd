{-|
Module: Text.XML.XSD.Validation.AttributeGroup
Description: @Attribute Group Definition@ schema component

The @Attribute Group Definition@ schema component

https://www.w3.org/TR/xmlschema-1/#Attribute_Group_Definition_details
-}

module Text.XML.XSD.Validation.AttributeGroup
  ( AttributeGroup(..)
  -- * Lenses
  , agName
  , agTargetNamespace
  , agAttrUses
  , agAttrWildcard
  )
  where

import Text.XML.XSD.Validation.Internal
