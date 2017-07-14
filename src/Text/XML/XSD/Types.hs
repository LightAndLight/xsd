{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language QuasiQuotes #-}
module Text.XML.XSD.Types where

import Prelude

import Control.Lens hiding (Choice, element)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)

import qualified Data.Text as T

import Text.XML.Attrs
import Text.XML.Boolean
import Text.XML.Decimal
import Text.XML.Float
import Text.XML.ID
import Text.XML.NCName
import Text.XML.NonNegative
import Text.XML.QName
import Text.XML.Regex
import Text.XML.Token
import Text.XML.URI
import Text.XML.XSD.Final
import Text.XML.XSD.Form

import qualified Data.Map as M
import qualified Text.XML as XML
import qualified Text.XML.Lens as XML

-- | XSD primitive datatypes
data PrimitiveType
  = PString
  | PBoolean
  | PDecimal
  | PFloat
  | PDouble
  | PDateTime
  | PTime
  | PDate
  | PGYearMonth
  | PGYear
  | PGMonthDay
  | PGDay
  | PGMonth
  | PHexBinary
  | PBase64Binary
  | PAnyURI
  | PQName
  | PNOTATION

-- | Some text and its associated XSD type
data AnySimpleType
  = AnySimpleType
  { _astValue :: Text
  , _astType :: PrimitiveType
  }

_AnySimpleType :: Prism' (Text, PrimitiveType) AnySimpleType
_AnySimpleType = prism' (\(AnySimpleType a b) -> (a, b)) $
  \(txt, ty) ->
    let
      test = case ty of
        PString -> const True
        PBoolean -> isBoolean 
        PDecimal -> isDecimal
        PFloat -> isFloat
        PDouble -> _
        PDateTime -> _
        PTime -> _
        PDate -> _
        PGYearMonth -> _
        PGYear -> _
        PGMonthDay -> _
        PGDay -> _
        PGMonth -> _
        PHexBinary -> _
        PBase64Binary -> _
        PAnyURI -> _
        PQName -> isQName
        PNOTATION -> _
     in if test txt then Just (AnySimpleType txt ty) else Nothing

-- | Permitted 'whiteSpace' 'value's
data WhiteSpaceSetting = Collapse | Replace | Preserve

data ConstraintFacet
  -- | 'length' element https://www.w3.org/TR/xmlschema-2/#element-length
  = CFLength
    { _cfID :: Maybe NCName
    , _cfLengthValue :: NonNegative
    , _cfLengthFixed :: Maybe Bool
    , _cfAttrs :: Attrs
    }
  
  -- | 'minLength' element https://www.w3.org/TR/xmlschema-2/#element-minLength
  | CFMinLength
    { _cfID :: Maybe NCName
    , _cfMinLengthValue :: NonNegative
    , _cfMinLengthFixed :: Maybe Bool
    , _cfAttrs :: Attrs
    }
  
  -- | 'maxLength' element https://www.w3.org/TR/xmlschema-2/#element-maxLength
  | CFMaxLength
    { _cfID :: Maybe NCName
    , _cfMaxLengthValue :: NonNegative
    , _cfMaxLengthFixed :: Maybe Bool
    , _cfAttrs :: Attrs
    }

  -- | 'pattern' element https://www.w3.org/TR/xmlschema-2/#element-pattern
  | CFPattern
    { _cfID :: Maybe NCName
    , _cfPatternValue :: Regex
    , _cfAttrs :: Attrs
    }
    
  -- | 'enumeration' element https://www.w3.org/TR/xmlschema-2/#element-enumeration
  | CFEnumeration
    { _cfID :: Maybe NCName
    , _cfEnumerationValue :: AnySimpleType
    , _cfAttrs :: Attrs
    }
    
  -- | 'whiteSpace' element https://www.w3.org/TR/xmlschema-2/#element-whiteSpace
  | CFWhiteSpace
    { _cfID :: Maybe NCName
    , _cfWhiteSpaceValue :: WhiteSpaceSetting
    , _cfWhiteSpaceFixed :: Maybe Bool
    , _cfAttrs :: Attrs
    }
    
  -- | 'maxInclusive' element https://www.w3.org/TR/xmlschema-2/#element-maxInclusive
  -- | 'minInclusive' element https://www.w3.org/TR/xmlschema-2/#element-minInclusive
  -- | 'maxExclusive' element https://www.w3.org/TR/xmlschema-2/#element-maxInclusive
  -- | 'minExclusive' element https://www.w3.org/TR/xmlschema-2/#element-minInclusive
  -- | 'totalDigits' element https://www.w3.org/TR/xmlschema-2/#element-totalDigits
  -- | 'fractionDigits' element https://www.w3.org/TR/xmlschema-2/#element-fractionDigits

cfAttrs :: Lens' ConstraintFacet Attrs
cfAttrs = lens _cfAttrs (\s a -> s { _cfAttrs = a })

instance HasAttrs ConstraintFacet where
  attrs = cfAttrs . attrs

constraintFacetElementName :: Getter ConstraintFacet XML.Name
constraintFacetElementName =
  to $ \case
    CFLength{} -> "length"
    CFMinLength{} -> "minLength"
    CFMaxLength{} -> "maxLength"
    CFPattern{} -> "pattern"
    CFEnumeration{} -> "enumeration"
    CFWhiteSpace{} -> "whiteSpace"

constraintFacetAttrs :: Getter ConstraintFacet [Maybe (QName, Text)]
constraintFacetAttrs =
  to $ \case
    CFLength{..} ->
      [ Just ([qn|value|], _NonNegative # _cfLengthValue)
      , (,) [qn|fixed|] . (_Boolean #) <$> _cfLengthFixed
      ]
    CFMinLength{..} ->
      [ Just ([qn|value|], _NonNegative # _cfMinLengthValue)
      , (,) [qn|fixed|] . (_Boolean #) <$> _cfMinLengthFixed
      ]
    CFMaxLength{..} ->
      [ Just ([qn|value|], _NonNegative # _cfMaxLengthValue)
      , (,) [qn|fixed|] . (_Boolean #) <$> _cfMaxLengthFixed
      ]
    CFPattern{..} ->
      [ Just ([qn|value|], _Regex # _cfPatternValue)
      ]
    CFEnumeration{..} -> _
    CFWhiteSpace{..} -> _
    
toElement
  :: HasAttrs a
  => Getter a XML.Name
  -> Getter a [Maybe (QName, Text)]
  -> Getter a [b]
  -> Getter b XML.Element
  -> a
  -> XML.Element
toElement getName maybeAttrs getContents getElement a =
  XML.Element
  { elementName = a ^. getName
  , elementAttributes =
      M.mapKeys qNameToName $
      (a ^. attrs) `M.union` M.fromList (a ^.. maybeAttrs . folded . _Just)
  , elementNodes = a ^.. getContents . folded . getElement . re XML._Element
  }

constraintFacetToElement :: ConstraintFacet -> XML.Element
constraintFacetToElement =
  toElement
    constraintFacetElementName
    constraintFacetAttrs
    (like [])
    (re _Void)

elementToConstraintFacet :: XML.Element -> Maybe ConstraintFacet
elementToConstraintFacet XML.Element{..} =
  case elementName of
    "length" -> _
    "minLength" -> _
    "maxLength" -> _
    "pattern" -> _
    "enumeration" -> _
    "whiteSpace" -> _
    

_ConstraintFacet :: Prism' XML.Element ConstraintFacet
_ConstraintFacet = prism' constraintFacetToElement elementToConstraintFacet

-- | 'include' element https://www.w3.org/TR/xmlschema-1/#element-include
data Include
  = Include
  { _incID :: Maybe NCName
  , _incSchemaLocation :: Maybe URI
  , _incAttrs :: Attrs
  }
  
-- | 'import' element https://www.w3.org/TR/xmlschema-1/#element-import
data Import
  = Import
  { _impID :: Maybe NCName
  , _impNamespace :: Maybe URI
  , _impSchemaLocation :: Maybe URI
  , _impAttrs :: Attrs
  }
  
-- | Permitted values when 'simpleType's 'final' attribute is a list
data STFFinal = STFList | STFUnion | STFRestriction

parseSTFFinal :: Text -> Maybe STFFinal
parseSTFFinal i =
  case i of
    "list" -> Just STFList
    "union" -> Just STFUnion
    "restriction" -> Just STFRestriction
    _ -> Nothing

showSTFFinal :: STFFinal -> Text
showSTFFinal i =
  case i of
    STFList -> "list"
    STFUnion -> "union"
    STFRestriction -> "restriction"

-- | Permitted values of 'simpleType's 'final' attribute
data STFinal = STAll | STMultiple [STFFinal]

parseSTFinal :: Text -> Maybe STFinal
parseSTFinal i =
  case i of
    "#all" -> Just STAll
    _ -> STMultiple <$> traverse parseSTFFinal (T.words i)

showSTFinal :: STFinal -> Text
showSTFinal a =
  case a of
    STAll -> "#all"
    STMultiple elems -> T.unwords $ fmap showSTFFinal elems

instance AsFinal Text STFinal where
  _Final = prism' showSTFinal parseSTFinal

-- | Enumeration for possible contents of 'simpleType' element 
-- | https://www.w3.org/TR/xmlschema-1/#element-simpleType
data STContent
  -- | Containing a 'restriction' element
  -- | https://www.w3.org/TR/xmlschema-1/#element-restriction
  = STRestriction
  { _stcID :: Maybe NCName
  , _stcAttrs :: Attrs
  , _strsBase :: Maybe QName
  , _strsConstraints :: [ConstraintFacet]
  }
  
  -- | Containing a 'list' element
  -- | https://www.w3.org/TR/xmlschema-1/#element-list
  | STList
  { _stcID :: Maybe NCName
  , _stcAttrs :: Attrs
  , _stlsItemType :: Maybe QName
  , _stlsTypeElement :: Maybe SimpleType
  }
  
  -- | Containing a 'union' element
  -- | https://www.w3.org/TR/xmlschema-1/#element-union
  | STUnion
  { _stcID :: Maybe NCName
  , _stcAttrs :: Attrs
  , _stunMemberTypes :: [QName]
  , _stunTypeElements :: [SimpleType]
  }

stcID :: Lens' STContent (Maybe NCName)
stcID = lens _stcID (\s a -> s { _stcID = a})

stcAttrs :: Lens' STContent Attrs
stcAttrs = lens _stcAttrs (\s a -> s { _stcAttrs = a})

stContentToElement :: STContent -> XML.Element
stContentToElement STRestriction{..} =
  XML.Element
  { elementName = "restriction"
  , elementAttributes =
      M.fromList (catMaybes
      [ (,) "id" <$> _stcID ^? _Just . re _NCName
      , (,) "base" <$> _strsBase ^? _Just . re _QName
      ]) `M.union`
      toNameTextMap _stcAttrs
  , elementNodes =
    _strsConstraints ^.. folded . re _ConstraintFacet . re XML._Element
  }
  
stContentToElement STList{..} = _
stContentToElement STUnion{..} = _

_SimpleTypeContent :: Prism' XML.Element STContent
_SimpleTypeContent = prism' stContentToElement _

-- | 'simpleType' element https://www.w3.org/TR/xmlschema-1/#element-simpleType
data SimpleType
  = SimpleType
  { _stID :: Maybe NCName
  , _stName :: Maybe NCName
  , _stFinal :: Maybe STFinal
  , _stContent :: STContent
  }
  
data CTBlock = CTBExtension | CTBRestriction
data CTFinal = CTFExtension | CTFRestriction

-- | Enumeration for possible contents of 'complexType' element https://www.w3.org/TR/xmlschema-1/#element-complexType
data CTContent

  -- | Containing a 'simpleContent' element https://www.w3.org/TR/xmlschema-1/#element-simpleContent
  = CTSimpleContent
  { _ctscID :: Maybe NCName
  , _ctscAttrs :: Attrs
  , _ctscContent :: Either SimpleRestriction SimpleExtension
  }

  -- | Containing a 'complexContent' element https://www.w3.org/TR/xmlschema-1/#element-complexContent
  | CTComplexContent
  { _ctccID :: Maybe NCName
  , _ctccAttrs :: Attrs
  , _ctccMixed :: Maybe Bool
  , _ctccContent :: Either ComplexRestriction ComplexExtension
  }

  -- | Containing an optional group definition and attribute specifications
  | CTGroupContent
  { _ctgdGroupDefinition :: Maybe CTGroupDefinition
  , _ctgdAttributeSpec :: [Either Attribute AttributeGroup]
  , _ctgdAnyAttribute :: Maybe AnyAttribute
  }
  

-- | 'complexType' element https://www.w3.org/TR/xmlschema-1/#element-complexType
data ComplexType
  = ComplexType
  { _ctID :: Maybe NCName
  , _ctAbstract :: Maybe Bool
  , _ctBlock :: Maybe CTBlock
  , _ctFinal :: Maybe CTFinal
  , _ctMixed :: Maybe Bool
  , _ctName :: Maybe NCName
  , _ctContent :: CTContent
  }

-- | Permitted content for 'redefine' elements
data RedefineContent
  = RCSimpleType SimpleType
  | RCComplexType ComplexType
  | RCGroup Group
  | RCAttributeGroup AttributeGroup
  
-- | 'redefine' element https://www.w3.org/TR/xmlschema-1/#element-redefine
data Redefine
  = Redefine
  { _redID :: Maybe NCName
  , _redSchemaLocation :: Maybe URI
  , _redAttrs :: Attrs
  , _redContent :: [RedefineContent]
  }
  
-- | 'notation' element https://www.w3.org/TR/xmlschema-1/#element-notation
data Notation
  = Notation
  { _notID :: Maybe NCName
  , _notName :: Maybe NCName
  , _notPublic :: Maybe Token
  , _notSystem :: Maybe URI
  , _notAttrs :: Attrs
  }
  
-- | Permitted values for an 'attribute's 'use' attribute
data Use = Optional | Prohibited | Required

-- | 'attribute' element https://www.w3.org/TR/xmlschema-1/#element-attribute 
data Attribute
  = Attribute
  { _attID :: Maybe NCName
  , _attDefault :: Maybe Text
  , _attFixed :: Maybe Text
  , _attForm :: Maybe Form
  , _attName :: Maybe NCName
  , _attRef :: Maybe QName
  , _attType :: Maybe QName
  , _attUse :: Maybe Use
  , _attAttrs :: Attrs
  , _attSimpleType :: Maybe SimpleType
  }

-- | 'attributeGroup' element https://www.w3.org/TR/xmlschema-1/#element-attributeGroup
data AttributeGroup
  = AttributeGroup
  { _agID :: Maybe NCName
  , _agName :: Maybe NCName
  , _agRef :: Maybe QName
  , _agAttrs :: Attrs
  , _agAttributeSpec :: [Either Attribute AttributeGroup]
  , _agAnyAttribute :: Maybe AnyAttribute
  }

-- | Upper bound on occurrances of things
data Occurances = Unbounded | Bounded NonNegative

-- | 'element' element https://www.w3.org/TR/xmlschema-1/#element-element
data Element
  = Element
  { _elID :: Maybe NCName
  , _elAbstract :: Maybe Bool
  , _elForm :: Maybe Form
  , _elMaxOccurs :: Maybe Occurances
  , _elMinOccurs :: Maybe NonNegative
  , _elName :: NCName
  , _elNillable :: Maybe Bool
  , _elTypeName :: Maybe QName
  , _elTypeElement :: Maybe (Either SimpleType ComplexType)
  , _elAttrs :: Attrs
  -- , _elSomethingKeywords
  -- , elRef :: QName
  -- , elSubstitutionGroup :: QName
  -- , elBlock :: ??
  -- , elDefault :: String
  }

mkElement :: NCName -> Element
mkElement name
  = Element
  { _elID = Nothing
  , _elAbstract = Nothing
  , _elForm = Nothing
  , _elMaxOccurs = Nothing
  , _elMinOccurs = Nothing
  , _elName = name
  , _elNillable = Nothing
  , _elTypeName = Nothing
  , _elTypeElement = Nothing
  , _elAttrs = emptyAttrs
  }

{-
class HasElement s where
  element :: Lens' s Element
  elID :: Lens' s (Maybe ID)
  elAbstract :: Lens' s (Maybe Bool)
  elForm :: Lens' s (Maybe Form)
  elMaxOccurs :: Lens' s (Maybe Occurances)
  elMinOccurs :: Lens' s (Maybe NonNegative)
  elName :: Lens' s Name
  elNillable :: Lens' s (Maybe Bool)
  elTypeName :: Lens' s (Maybe QName)
  elTypeElement :: Lens' s (Either SimpleType ComplexType)
  elAttrs :: Lens' s (Maybe Text Text)
-} 

class AsElement s where
  _Element :: NCName -> Review s Element
  _Element' :: Prism' s Element

-- | Permitted values of when 'namespace' attribute is a list
data Locality = TargetNamespace | Local

-- | Permitted values of the 'namespace' attribute
data Namespace = NSAny | NSOther | NSList [Either URI Locality]

-- | Permitted values of the 'processContents' attribute
data ProcessContents = Lax | Skip | Strict

-- | 'anyAttribute' element https://www.w3.org/TR/xmlschema-1/#element-anyAttribute
data AnyAttribute
  = AnyAttribute
  { _aaID :: Maybe NCName
  , _aaNamespace :: Maybe Namespace
  , _aaProcessContents :: Maybe ProcessContents
  , _aaAttrs :: Attrs
  }

-- | 'attributeGroup' element within a 'restriction' element within a 'simpleContent'
-- | element https://www.w3.org/TR/xmlschema-1/#element-simpleContent..attributeGroup
data SimpleAttributeGroup
  = SimpleAttributeGroup
  { _sagID :: Maybe NCName
  , _sagRef :: Maybe QName
  , _sagAttrs :: Attrs
  }

-- | 'restriction' element within a 'simpleContent' element https://www.w3.org/TR/xmlschema-1/#element-simpleContent..restriction
data SimpleRestriction
  = SimpleRestriction
  { _srsBase :: Maybe QName
  , _srsID :: Maybe NCName
  , _srsAttrs :: Attrs
  , _srsType :: Maybe SimpleType
  , _srsConstraints :: [ConstraintFacet]
  , _srsAttributeSpec :: [Either Attribute SimpleAttributeGroup]
  , _srsAnyAttribute :: Maybe AnyAttribute
  }

-- | 'extension' element within a 'simpleContent' element https://www.w3.org/TR/xmlschema-1/#element-simpleContent..restriction
data SimpleExtension
  = SimpleExtension
  { _sexBase :: Maybe QName
  , _sexID :: Maybe NCName
  , _sexAttributeSpec :: [Either Attribute SimpleAttributeGroup]
  , _sexAnyAttribute :: Maybe AnyAttribute
  }

-- | 1
data One = One

-- | 0
data Zero = Zero

-- | 'all' element https://www.w3.org/TR/xmlschema-1/#element-all
data All
  = All
  { _allID :: Maybe NCName
  , _allMaxOccurs :: One
  , _allMinOccurs :: Either Zero One
  , _allAttrs :: Attrs
  , _allContent :: [Element]
  }

-- | 'any' element https://www.w3.org/TR/xmlschema-1/#element-all
data Any
  = Any
  { _anyID :: Maybe NCName
  , _anyMaxOccurs :: Occurances
  , _anyMinOccurs :: NonNegative
  , _anyNamespace :: Maybe Namespace
  , _anyProcessContents :: ProcessContents
  , _anyAttrs :: Attrs
  }

-- | Permitted content of a 'choice' element
data ChoiceContent
  = CCElement Element
  | CCGroup Group
  | CCSequence Sequence
  | CCAny Any
  
-- | 'choice' element https://www.w3.org/TR/xmlschema-1/#element-choice
data Choice
  = Choice
  { _choiceID :: Maybe NCName
  , _choiceMaxOccurs :: Occurances
  , _choiceMinOccurs :: NonNegative
  , _choiceAttrs :: Attrs
  , _choiceContent :: [ChoiceContent]
  }

-- | Permitted content of a 'sequence' element
data SequenceContent
  = SCElement Element
  | SCGroup Group
  | SCSequence Sequence
  | SCAny Any

instance AsElement SequenceContent where
  _Element name = unto (\el -> SCElement el { _elName = name })
  _Element' = prism' SCElement $
    \case
      SCElement a -> Just a
      _ -> Nothing
  
-- | 'sequence' element https://www.w3.org/TR/xmlschema-1/#element-sequence
data Sequence
  = Sequence
  { _sequenceID :: Maybe NCName
  , _sequenceMaxOccurs :: Maybe Occurances
  , _sequenceMinOccurs :: Maybe NonNegative
  , _sequenceAttrs :: Attrs
  , _sequenceContent :: [SequenceContent]
  }

class AsSequence s where
  _Sequence :: Prism' s Sequence

-- | Permitted content of a 'group' element
data GroupContent
  = GCAll All
  | GCChoice Choice
  | GCSequence Sequence

-- | 'group' element https://www.w3.org/TR/xmlschema-1/#element-group
data Group
  = Group
  { _grID :: Maybe NCName
  , _grMaxOccurs :: Maybe Occurances
  , _grMinOccurs :: Maybe NonNegative
  , _grName :: Maybe NCName
  , _grRef :: Maybe QName
  , _grAttrs :: Attrs
  , _grContent :: Maybe GroupContent
  }

-- | Permitted group definition elements in 'complexType' 
data CTGroupDefinition
  = CTGDGroup Group
  | CTGDAll All
  | CTGDChoice Choice
  | CTGDSequence Sequence

instance AsSequence CTGroupDefinition where
  _Sequence = prism'
    CTGDSequence $
    \case
      CTGDSequence a -> Just a
      _ -> Nothing

-- | 'extension' element within a 'complexContent' element https://www.w3.org/TR/xmlschema-1/#element-complexContent..extension
data ComplexExtension
  = ComplexExtension
  { _cexID :: Maybe NCName
  , _cexBase :: Maybe QName
  , _cexAttrs :: Attrs
  , _cexGroupDefinition :: Maybe CTGroupDefinition
  , _cexAttributeSpec :: [Either Attribute AttributeGroup]
  , _cexAnyAttribute :: Maybe AnyAttribute
  }

-- | 'restriction' element within a 'complexContent' element https://www.w3.org/TR/xmlschema-1/#element-complexContent..restriction
data ComplexRestriction
  = ComplexRestriction
  { _cerID :: Maybe NCName
  , _cerBase :: Maybe QName
  , _cerAttrs :: Attrs
  , _cerGroupDefinition :: Maybe CTGroupDefinition
  , _cerAttributeSpec :: [Either Attribute AttributeGroup]
  , _cerAnyAttribute :: Maybe AnyAttribute
  }

instance AsSequence CTContent where
  _Sequence = prism'
    (\v -> CTGroupContent
      { _ctgdGroupDefinition = Just $ _Sequence # v
      , _ctgdAttributeSpec = []
      , _ctgdAnyAttribute = Nothing
      })
    (maybe Nothing (^? _Sequence) . _ctgdGroupDefinition)

class AsComplexType s where
  _ComplexType :: Prism' s ComplexType

makeClassy ''Element
