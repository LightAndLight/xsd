{-#
language

LambdaCase, MultiParamTypeClasses, OverloadedStrings, TemplateHaskell,
RecordWildCards, QuasiQuotes
#-}

module Text.XML.XSD.Schema
  ( schema
  , Schema(..)
  , _Schema
  , SchemaPrelude(..)
  , SchemaElement(..)
  , _SchemaElement
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

import Prelude

import Control.Applicative
import Control.Lens
import Data.Text (Text)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.XML as XML
import qualified Text.XML.Lens as XML

import Text.XML.Attrs
import Text.XML.XSD.Types.Language
import Text.XML.XSD.Types.NCName
import Text.XML.XSD.Types.QName
import Text.XML.XSD.Types.Token
import Text.XML.XSD.Types.URI
import Text.XML.XSD.Block
import Text.XML.XSD.Final
import Text.XML.XSD.Form
import Text.XML.XSD.Internal.Types
import Text.XML.XSD.Internal.Lenses

-- | Permitted 'schemaBlockDefault' values when specifiying multiples
data SchemaBlock = SBExtension | SBRestriction | SBSubstitution
  deriving (Eq, Show)

-- | Permitted 'schemaBlockDefault' values
data SchemaBlockDefault = SBAll | SBMultiple [SchemaBlock]
  deriving (Eq, Show)

instance AsBlock Text SchemaBlockDefault where
  _Block = prism' showSchemaBlockDefault parseSchemaBlockDefault
    where
      showSchemaBlock :: SchemaBlock -> Text
      showSchemaBlock a =
        case a of
          SBExtension -> "extension"
          SBRestriction -> "restriction"
          SBSubstitution -> "substitution"

      parseSchemaBlock :: Text -> Maybe SchemaBlock
      parseSchemaBlock a =
        case a of
          "extension" -> Just SBExtension
          "restriction" -> Just SBRestriction
          "substitution" -> Just SBSubstitution
          _ -> Nothing
          
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

-- | Permitted 'schemaFinalDefault' values when specifiying multiples
data SchemaFinal = SFExtension | SFRestriction | SFList | SFUnion
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

_SchemaPrelude :: Prism' XML.Element SchemaPrelude
_SchemaPrelude =
  prism'
    (\case
        SPInclude i -> review _Include i
        SPImport i -> review _Import i
        SPRedefine r -> review _Redefine r)

    (\sp ->
        (SPInclude <$> preview _Include sp) <|>
        (SPImport <$> preview _Import sp) <|>
        (SPRedefine <$> preview _Redefine sp))

-- | Permitted 'schema' element values.
data SchemaElement
  = SESimpleType SimpleType
  | SEComplexType ComplexType
  | SEGroup Group
  | SEAttributeGroup AttributeGroup
  | SEElement Element
  | SEAttribute Attribute
  | SENotation Notation
  deriving (Eq, Show)

schemaElementToElement :: SchemaElement -> XML.Element
schemaElementToElement e =
  case e of
    SESimpleType a -> review _SimpleType a
    SEComplexType a -> review _ComplexType a
    SEGroup a -> review _Group a
    SEAttributeGroup a -> review _AttributeGroup a
    SEElement a -> review _Element a
    SEAttribute a -> review _Attribute a
    SENotation a -> review _Notation a

elementToSchemaElement :: XML.Element -> Maybe SchemaElement
elementToSchemaElement e =
  case XML.nameLocalName (XML.elementName e) of
    "simpleType" -> fmap SESimpleType (e ^? _SimpleType)
    "complexType" -> fmap SEComplexType (e ^? _ComplexType)
    "group" -> fmap SEGroup (e ^? _Group)
    "attributeGroup" -> fmap SEAttributeGroup (e ^? _AttributeGroup)
    "element" -> fmap SEElement (e ^? _Element)
    "attribute" -> fmap SEAttribute (e ^? _Attribute)
    "notation" -> fmap SENotation (e ^? _Notation)
    _ -> Nothing

_SchemaElement :: Prism' XML.Element SchemaElement
_SchemaElement = prism' schemaElementToElement elementToSchemaElement

instance AsSimpleType SchemaElement where
  _SimpleType =
    prism' SESimpleType $
    \case
      SESimpleType a -> Just a
      _ -> Nothing

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
  } deriving (Eq, Show)

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

_Schema :: Prism' XML.Element Schema
_Schema = prism' se es
  where
    se :: Schema -> XML.Element
    se = toElement
      ToElement
      { teName = const "schema"
      , teAttrs = \Schema{..} ->
        [ (,) [qn|id|] . review _NCName <$> _schemaID
        , (,) [qn|attributeFormDefault|] . review _Form <$> _schemaAttributeFormDefault
        , (,) [qn|blockDefault|] . review _Block <$> _schemaBlockDefault
        , (,) [qn|elementFormDefault|] . review _Form <$> _schemaElementFormDefault
        , (,) [qn|finalDefault|] . review _Final <$> _schemaFinalDefault
        , (,) [qn|targetNamespace|] . review _URI <$> _schemaTargetNamespace
        , (,) [qn|version|] . review _Token <$> _schemaVersion
        , (,) [qn|xml:lang|] . review _Language <$> _schemaXMLLang
        ]
      , teContents =
        [ Fold $ schemaPrelude . folded . re _SchemaPrelude
        , Fold $ schemaBody . folded . re _SchemaElement
        ]
      }

    es :: XML.Element -> Maybe Schema
    es XML.Element{..} =
      case XML.nameLocalName elementName of
        "schema" ->
          let
            _schemaID =
              elementAttributes ^? at "id" . _Just . _NCName
            _schemaAttributeFormDefault =
              elementAttributes ^? at "attributeFormDefault" . _Just . _Form
            _schemaBlockDefault =
              elementAttributes ^? at "blockDefault" . _Just . _Block
            _schemaElementFormDefault =
              elementAttributes ^? at "elementFormDefault" . _Just . _Form
            _schemaFinalDefault =
              elementAttributes ^? at "finalDefault" . _Just . _Final
            _schemaTargetNamespace =
              elementAttributes ^? at "targetNamespace" . _Just . _URI
            _schemaVersion =
              elementAttributes ^? at "version" . _Just . _Token
            _schemaXMLLang =
              elementAttributes ^? at "xml:lang" . _Just . _Language
            _schemaAttrs =
              emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
            _schemaPrelude =
              elementNodes ^..
              folded .
              XML._Element .
              _SchemaPrelude
            _schemaBody =
              elementNodes ^..
              folded .
              XML._Element .
              _SchemaElement
              
          in
          Just Schema{..}
        _ -> Nothing 
