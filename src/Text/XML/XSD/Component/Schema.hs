{-# language TemplateHaskell #-}

module Text.XML.XSD.Component.Schema
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

import Text.XML.XSD.Component.Attribute
import Text.XML.XSD.Component.AttributeGroup
import Text.XML.XSD.Component.ComplexType
import Text.XML.XSD.Component.Element
import Text.XML.XSD.Component.ModelGroup
import Text.XML.XSD.Component.Notation
import Text.XML.XSD.Component.SimpleType

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
