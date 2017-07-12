{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
module Text.XML.XSD.Types where

import Prelude

import Control.Lens hiding (Choice, element)
import Data.Text (Text)

import qualified Data.Text as T

import Text.XML.Attrs
import Text.XML.NCName
import Text.XML.QName
import Text.XML.Token
import Text.XML.URI
import Text.XML.XSD.Final
import Text.XML.XSD.Form

-- | XSD primitive datatypes
data XSDataType
  = TString
  | TBoolean
  | TDecimal
  | TFloat
  | TDouble
  | TDateTime
  | TTime
  | TDate
  | TGYearMonth
  | TGYear
  | TGMonthDay
  | TGDay
  | TGMonth
  | THexBinary
  | TBase64Binary
  | TAnyURI
  | TQName
  | TNOTATION

-- | Valid regular expression
type Regex = Text

-- | Valid XML attribute name
type Name = Text

-- | Non-negative integer
newtype NonNegative = NonNegative Int
  deriving Num

-- | Some text and its associated XSD type
data AnySimpleType
  = AnySimpleType
  { _astValue :: Text
  , _astType :: XSDataType
  }

-- | 'length'-specific attributes
data Length
  = Length
  { _lnValue :: NonNegative
  , _lnFixed :: Bool
  }
  
-- | 'minLength'-specific attributes
data MinLength
  = MinLength
  { _mnlnValue :: NonNegative
  , _mnlnFixed :: Bool
  }
  
-- | 'maxLength'-specific attributes
data MaxLength
  = MaxLength
  { _mxlnValue :: NonNegative
  , _mxlnFixed :: Bool
  }

-- | 'pattern'-specific attributes
data Pattern
  = Pattern
  { _ptValue :: Regex
  }

-- | 'enumeration'-specific attributes
data Enumeration
  = Enumeration
  { _enValue :: AnySimpleType
  }

-- | Permitted 'whiteSpace' 'value's
data WhiteSpaceSetting = Collapse | Replace | Preserve

-- | 'whiteSpace'-specific attributes
data WhiteSpace
  = WhiteSpace
  { _wsValue :: WhiteSpaceSetting
  , _wsFixed :: Bool
  }

data ConstraintFacet
  -- | 'length' element https://www.w3.org/TR/xmlschema-2/#element-length
  = CFLength
    { _cfID :: Maybe NCName
    , _cfLength :: Length
    , _cfAttrs :: Attrs
    }
  
  -- | 'minLength' element https://www.w3.org/TR/xmlschema-2/#element-minLength
  | CFMinLength
    { _cfID :: Maybe NCName
    , _cfMinLength :: MinLength
    , _cfAttrs :: Attrs
    }
  
  -- | 'maxLength' element https://www.w3.org/TR/xmlschema-2/#element-maxLength
  | CFMaxLength
    { _cfID :: Maybe NCName
    , _cfMaxLength :: MaxLength
    , _cfAttrs :: Attrs
    }

  -- | 'pattern' element https://www.w3.org/TR/xmlschema-2/#element-pattern
  | CFPattern
    { _cfID :: Maybe NCName
    , _cfPattern :: Pattern
    , _cfAttrs :: Attrs
    }
    
  -- | 'enumeration' element https://www.w3.org/TR/xmlschema-2/#element-enumeration
  | CFEnumeration
    { _cfID :: Maybe NCName
    , _cfEnumeration :: Enumeration
    , _cfAttrs :: Attrs
    }
    
  -- | 'whiteSpace' element https://www.w3.org/TR/xmlschema-2/#element-whiteSpace
  | CFWhiteSpace
    { _cfID :: Maybe NCName
    , _cfWhiteSpace :: WhiteSpace
    , _cfAttrs :: Attrs
    }
    
  -- | 'maxInclusive' element https://www.w3.org/TR/xmlschema-2/#element-maxInclusive
  -- | 'minInclusive' element https://www.w3.org/TR/xmlschema-2/#element-minInclusive
  -- | 'maxExclusive' element https://www.w3.org/TR/xmlschema-2/#element-maxInclusive
  -- | 'minExclusive' element https://www.w3.org/TR/xmlschema-2/#element-minInclusive
  -- | 'totalDigits' element https://www.w3.org/TR/xmlschema-2/#element-totalDigits
  -- | 'fractionDigits' element https://www.w3.org/TR/xmlschema-2/#element-fractionDigits

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
  { strsBase :: Maybe QName
  , strsID :: Maybe NCName
  , strsAttrs :: Attrs
  , strsConstraints :: [ConstraintFacet]
  }
  
  -- | Containing a 'list' element
  -- | https://www.w3.org/TR/xmlschema-1/#element-list
  | STList
  { stlsID :: Maybe NCName
  , stlsItemType :: Maybe QName
  , stlsAttrs :: Attrs
  , stlsTypeElement :: Maybe SimpleType
  }
  
  -- | Containing a 'union' element
  -- | https://www.w3.org/TR/xmlschema-1/#element-union
  | STUnion
  { stunID :: Maybe NCName
  , stunMemberTypes :: [QName]
  , stunAttrs :: Attrs
  , stunTypeElements :: [SimpleType]
  }

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
