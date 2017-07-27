{-#
language

LambdaCase, MultiParamTypeClasses, OverloadedStrings, TemplateHaskell,
RecordWildCards, QuasiQuotes, TypeSynonymInstances, FlexibleInstances
#-}

module Text.XML.XSD.XMLRep.Schema
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
  , schemaNamespace
  )
  where

import Prelude

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Bifunctor
import Data.Monoid
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

schemaURI :: URI
schemaURI = [uri|https://www.w3.org/2001/XMLSchema|]

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

_SchemaPrelude :: Prism' XML.Element (Namespaced SchemaPrelude)
_SchemaPrelude =
  prism'
    (\case
        (ns, SPInclude i) -> review _Include (ns, i)
        (ns, SPImport i) -> review _Import (ns, i)
        (ns, SPRedefine r) -> review _Redefine (ns, r))

    (\sp ->
        (fmap SPInclude <$> preview _Include sp) <|>
        (fmap SPImport <$> preview _Import sp) <|>
        (fmap SPRedefine <$> preview _Redefine sp))

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

_SchemaElement :: Prism' XML.Element (Namespaced SchemaElement)
_SchemaElement = prism' schemaElementToElement elementToSchemaElement
  where
    schemaElementToElement :: Namespaced SchemaElement -> XML.Element
    schemaElementToElement =
      \case
        (ns, SESimpleType a) -> review _SimpleType (ns, a)
        (ns, SEComplexType a) -> review _ComplexType (ns, a)
        (ns, SEGroup a) -> review _Group (ns, a)
        (ns, SEAttributeGroup a) -> review _AttributeGroup (ns, a)
        (ns, SEElement a) -> review _Element (ns, a)
        (ns, SEAttribute a) -> review _Attribute (ns, a)
        (ns, SENotation a) -> review _Notation (ns, a)

    elementToSchemaElement :: XML.Element -> Maybe (Namespaced SchemaElement)
    elementToSchemaElement e = do
      case XML.nameLocalName (XML.elementName e) of
        "simpleType" -> fmap SESimpleType <$> (e ^? _SimpleType)
        "complexType" -> fmap SEComplexType <$> (e ^? _ComplexType)
        "group" -> fmap SEGroup <$> (e ^? _Group)
        "attributeGroup" -> fmap SEAttributeGroup <$> (e ^? _AttributeGroup)
        "element" -> fmap SEElement <$> (e ^? _Element)
        "attribute" -> fmap SEAttribute <$> (e ^? _Attribute)
        "notation" -> fmap SENotation <$> (e ^? _Notation)
        _ -> Nothing

instance AsSimpleType (Namespaced SchemaElement) where
  _SimpleType =
    prism' (fmap SESimpleType) $
    \case
      (ns, SESimpleType a) -> Just (ns, a)
      _ -> Nothing

instance AsComplexType (Namespaced SchemaElement) where
  _ComplexType =
    prism' (fmap SEComplexType) $
    \case
      (ns, SEComplexType a) -> Just (ns, a)
      _ -> Nothing
      
instance AsComplexType SchemaElement where
  _ComplexType =
    prism' (SEComplexType . snd) $
    \case
      SEComplexType a -> Just (Nothing, a)
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
  , _schemaNamespace :: (Maybe NCName, URI)
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
  , _schemaNamespace = (Nothing, schemaURI)
  }

makeLenses ''Schema

instance HasAttrs Schema where
  attrs = schemaAttrs . attrs

_Schema :: Prism' XML.Element Schema
_Schema = prism' se es
  where
    se :: Schema -> XML.Element
    se s = toElement
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
        , liftA2 (,)
          (preview _QName $ "xmlns" <>
            (maybe "" (":" <>) . fmap (review _NCName) $ fst _schemaNamespace))
          (pure $ _URI # snd _schemaNamespace) 
        ]
      , teContents =
        [ TEContent (Fold $ schemaPrelude . folded) (Fold $ re _SchemaPrelude)
        , TEContent (Fold $ schemaBody . folded) (Fold $ re _SchemaElement)
        ]
      }
      (fst $ _schemaNamespace s, s)

    es :: XML.Element -> Maybe Schema
    es XML.Element{..} =
      case XML.nameLocalName elementName of
        "schema" -> do
          let
            reservedKeys =
              [ "id"
              , "attributeFormDefault"
              , "blockDefault"
              , "elementFormDefault"
              , "finalDefault"
              , "targetNamespace"
              , "version"
              , "xml:lang"
              ]
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
              createAttrs reservedKeys $
              M.filterWithKey
                (\k _ -> XML.nameNamespace k /= Just "xmlns")
                elementAttributes
            _schemaPrelude =
              elementNodes ^..
              folded .
              XML._Element .
              _SchemaPrelude . _2
            _schemaBody =
              elementNodes ^..
              folded .
              XML._Element .
              _SchemaElement . _2
            ns_qual =
              M.toList elementAttributes ^?
              folded .
              filtered ((==) (Just "xmlns") . XML.nameNamespace . fst) .
              to (first $ preview _NCName . XML.nameLocalName)
            ns_unqual = (,) Nothing <$> M.lookup "xmlns" elementAttributes
          _schemaNamespace <-
            join $ sequence . fmap (preview _URI) <$> (ns_qual <|> ns_unqual)
          Just Schema{..}
        _ -> Nothing 
