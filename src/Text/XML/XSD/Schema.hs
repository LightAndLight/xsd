{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}

module Text.XML.XSD.Schema
  ( schema
  , Schema(..)
  , SchemaPrelude(..)
  , SchemaElement(..)
  , schemaElementTags
  -- ^ Lenses
  , schemaID
  , schemaAttributeFormDefault
  , schemaBlockDefault
  , schemaElementFormDefault
  , schemaFinalDefault
  , schemaTargetNamespace
  , schemaVersion
  , schemaXMLLang
  , schemaAttrs
  , schemaPrelude 
  , schemaBody 
  )
  where

import Prelude (Maybe(..), ($), (.))

import Control.Lens
import Data.CaseInsensitive (CI)
import Data.Text (Text)

import Text.XML.Attrs
import Text.XML.NCName
import Text.XML.XSD.Form
import Text.XML.XSD.Types

-- | Permitted 'schemaBlockDefault' values when specifiying multiples
data SchemaBlock = SBExtension | SBRestriction | SBSubstitution

-- | Permitted 'schemaBlockDefault' values
data SchemaBlockDefault = SBAll | SBMultiple [SchemaBlock]

-- | Permitted 'schemaFinalDefault' values when specifiying multiples
data SchemaFinal = SFExtension | SFRestriction | SFList | SFUnion

-- | Permitted 'schemaFinalDefault' values
data SchemaFinalDefault = SFAll | SFMultiple [SchemaFinal]

-- | Permitted 'schema' prelude values.
data SchemaPrelude
  = SPInclude Include
  | SPImport Import
  | SPRedefine Redefine

-- | Permitted 'schema' element values.
data SchemaElement
  = SESimpleType SimpleType
  | SEComplexType ComplexType
  | SEGroup Group
  | SEAttributeGroup AttributeGroup
  | SEElement Element
  | SEAttribute Attribute
  | SENotation Notation

schemaElementTags :: [CI Text]
schemaElementTags =
  [ "simpleType"
  , "complexType"
  , "group"
  , "attributeGroup"
  , "element"
  , "attribute"
  , "notation"
  ]

instance AsComplexType SchemaElement where
  _ComplexType =
    prism' SEComplexType $
    \case
      SEComplexType a -> Just a
      _ -> Nothing

-- | 'schema' element https://www.w3.org/TR/xmlschema-1/#element-schema 
data Schema
  = Schema
  { _schemaID :: Maybe NCName
  , _schemaAttributeFormDefault :: Maybe Form
  , _schemaBlockDefault :: Maybe SchemaBlockDefault
  , _schemaElementFormDefault :: Maybe Form
  , _schemaFinalDefault :: Maybe SchemaFinalDefault
  , _schemaTargetNamespace :: Maybe URI
  , _schemaVersion :: Maybe Token
  , _schemaXMLLang :: Maybe Language
  , _schemaAttrs :: Attrs
  , _schemaPrelude :: [SchemaPrelude]
  , _schemaBody :: [SchemaElement]
  }

-- | An empty 'schema' element
schema :: [SchemaElement] -> Schema
schema content
  = Schema
  { _schemaID = Nothing
  , _schemaAttributeFormDefault = Nothing
  , _schemaBlockDefault = Nothing
  , _schemaElementFormDefault = Nothing
  , _schemaFinalDefault = Nothing
  , _schemaTargetNamespace = Nothing
  , _schemaVersion = Nothing
  , _schemaXMLLang = Nothing
  , _schemaAttrs = emptyAttrs
  , _schemaPrelude = []
  , _schemaBody = content
  }

makeLenses ''Schema

instance HasAttrs Schema where
  attrs = schemaAttrs . attrs
