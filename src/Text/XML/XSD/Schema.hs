{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}

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

import Prelude (Maybe(..), ($), (.), fmap, (<$>))

import Control.Lens
import Data.CaseInsensitive (CI)
import Data.Text (Text)

import qualified Data.Text as T

import Text.XML.Attrs
import Text.XML.Language
import Text.XML.NCName
import Text.XML.Token
import Text.XML.URI
import Text.XML.XSD.Block
import Text.XML.XSD.Final
import Text.XML.XSD.Form
import Text.XML.XSD.Types

-- | Permitted 'schemaBlockDefault' values when specifiying multiples
data SchemaBlock = SBExtension | SBRestriction | SBSubstitution

showSchemaBlock :: SchemaBlock -> Text
showSchemaBlock a =
  case a of
    SBExtension -> "extension"
    SBRestriction -> "restrction"
    SBSubstitution -> "substitution"

parseSchemaBlock :: Text -> Maybe SchemaBlock
parseSchemaBlock a =
  case a of
    "extension" -> Just SBExtension
    "restriction" -> Just SBRestriction
    "substitution" -> Just SBSubstitution
    _ -> Nothing

-- | Permitted 'schemaBlockDefault' values
data SchemaBlockDefault = SBAll | SBMultiple [SchemaBlock]

showSchemaBlockDefault :: SchemaBlockDefault -> Text
showSchemaBlockDefault a =
  case a of
    SBAll -> "#all"
    SBMultiple elems -> T.unwords $ fmap showSchemaBlock elems

parseSchemaBlockDefault :: Text -> Maybe SchemaBlockDefault
parseSchemaBlockDefault a =
  case a of
    "" -> Just $ SBMultiple []
    "#all" -> Just SBAll
    _ -> SBMultiple <$> traverse parseSchemaBlock (T.words a)

instance AsBlock Text SchemaBlockDefault where
  _Block = prism' showSchemaBlockDefault parseSchemaBlockDefault

-- | Permitted 'schemaFinalDefault' values when specifiying multiples
data SchemaFinal = SFExtension | SFRestriction | SFList | SFUnion

showSchemaFinal :: SchemaFinal -> Text
showSchemaFinal sf =
  case sf of
    SFExtension -> "extension"
    SFRestriction -> "restriction"
    SFList -> "list"
    SFUnion -> "union"

parseSchemaFinal :: Text -> Maybe SchemaFinal
parseSchemaFinal input =
  case input of
    "extension" -> Just SFExtension
    "restriction" -> Just SFRestriction
    "list" -> Just SFList
    "union" -> Just SFUnion
    _ -> Nothing

-- | Permitted 'schemaFinalDefault' values
data SchemaFinalDefault = SFAll | SFMultiple [SchemaFinal]

showSchemaFinalDefault :: SchemaFinalDefault -> Text
showSchemaFinalDefault a =
  case a of
    SFAll -> "#all"
    SFMultiple elems -> T.unwords $ fmap showSchemaFinal elems

parseSchemaFinalDefault :: Text -> Maybe SchemaFinalDefault
parseSchemaFinalDefault a =
  case a of
    "" -> Just $ SFMultiple []
    "#all" -> Just SFAll
    _ -> SFMultiple <$> traverse parseSchemaFinal (T.words a)

instance AsFinal Text SchemaFinalDefault where
  _Final = prism' showSchemaFinalDefault parseSchemaFinalDefault

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
