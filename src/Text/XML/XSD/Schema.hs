{-# language LambdaCase #-}
{-# language TemplateHaskell #-}

module Text.XML.XSD.Schema
  ( schema
  , schemaBody
  , schemaEntry
  , Schema(..)
  , SchemaPrelude(..)
  , SchemaEntry(..)
  , SchemaBody(..)
  , SchemaElement(..)
  -- ^ Lenses
  , sbElement
  , sbAnnotation
  , sePrelude
  , seBody
  , schemaID
  , schemaAttributeFormDefault
  , schemaBlockDefault
  , schemaElementFormDefault
  , schemaFinalDefault
  , schemaTargetNamespace
  , schemaVersion
  , schemaXMLLang
  , schemaAttrs
  , schemaContent 
  )
  where

import Prelude (Maybe(..), ($), (.))

import Control.Lens hiding (element)
import Data.Map (Map)
import Data.Text (Text)

import qualified Data.Map as M

import Text.XML.Attrs
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
  | SPAnnotation Annotation

instance AsComplexType SchemaElement where
  _ComplexType =
    prism' SEComplexType $
    \case
      SEComplexType a -> Just a
      _ -> Nothing

-- | Permitted 'schema' element values.
data SchemaElement
  = SESimpleType SimpleType
  | SEComplexType ComplexType
  | SEGroup Group
  | SEAttributeGroup AttributeGroup
  | SEElement Element
  | SEAttribute Attribute
  | SENotation Notation

-- | A 'schema' body section
data SchemaBody
  = SchemaBody
  { _sbElement :: SchemaElement
  , _sbAnnotation :: [Annotation]
  }
  
schemaBody :: SchemaElement -> SchemaBody
schemaBody el
  = SchemaBody
  { _sbElement = el
  , _sbAnnotation = []
  }
  
makeLenses ''SchemaBody

instance AsComplexType SchemaBody where
  _ComplexType =
    prism' (schemaBody . review _ComplexType) $
    (\v -> _sbElement v ^? _ComplexType)


  

-- | A 'schema' content block
data SchemaEntry
  = SchemaEntry
  { _sePrelude :: [SchemaPrelude]
  , _seBody :: [SchemaBody]
  }

makeLenses ''SchemaEntry

schemaEntry :: SchemaEntry
schemaEntry
  = SchemaEntry
  { _sePrelude = []
  , _seBody = []
  }

-- | 'schema' element https://www.w3.org/TR/xmlschema-1/#element-schema 
data Schema
  = Schema
  { _schemaID :: Maybe ID
  , _schemaAttributeFormDefault :: Maybe Form
  , _schemaBlockDefault :: Maybe SchemaBlockDefault
  , _schemaElementFormDefault :: Maybe Form
  , _schemaFinalDefault :: Maybe SchemaFinalDefault
  , _schemaTargetNamespace :: Maybe URI
  , _schemaVersion :: Maybe Token
  , _schemaXMLLang :: Maybe Language
  , _schemaAttrs :: Attrs
  , _schemaContent :: [SchemaEntry]
  }

-- | An empty 'schema' element
schema :: [SchemaEntry] -> Schema
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
  , _schemaContent = content
  }

makeLenses ''Schema

instance HasAttrs Schema where
  attrs = schemaAttrs . attrs
