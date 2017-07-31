{-|
Module: Text.XML.XSD.Validation.Schema
Description: @Schema@ schema component

The @Schema@ schema component

https://www.w3.org/TR/xmlschema-2/#Schema_details
-}

{-# language TemplateHaskell #-}
module Text.XML.XSD.Validation.Schema
  ( Schema(..)
  -- * Lenses
  , schemaTypes
  , schemaAttrs
  , schemaElements
  , schemaAttrGroups
  , schemaModelGroups
  , schemaNotations
  )
  where

import Prelude

import Control.Lens (makeLenses)

import Text.XML.XSD.Validation.Attribute
import Text.XML.XSD.Validation.AttributeGroup
import Text.XML.XSD.Validation.ComplexType
import Text.XML.XSD.Validation.Element
import Text.XML.XSD.Validation.ModelGroup
import Text.XML.XSD.Validation.Notation
import Text.XML.XSD.Validation.SimpleType

data Schema
  = Schema
  { _schemaTypes :: [Either SimpleType ComplexType]
  , _schemaAttrs :: [Attribute]
  , _schemaElements :: [Element]
  , _schemaAttrGroups :: [AttributeGroup]
  , _schemaModelGroups :: [ModelGroup]
  , _schemaNotations :: [Notation]
  }

makeLenses ''Schema
