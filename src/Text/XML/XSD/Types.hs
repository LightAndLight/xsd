{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language TemplateHaskell #-}
module Text.XML.XSD.Types where

import Prelude

import Control.Lens (Lens', Prism', Review, prism', unto, (#), (^?), makeClassy)
import Data.Map (Map)
import Data.Text (Text)

import qualified Data.Map as M

import Text.XML.Attrs
import Text.XML.NCName
import Text.XML.QName

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

-- | XML ID
type ID = Text

-- | Valid XML URI
type URI = Text

-- | Valid langage
type Language = Text

-- | Valid regular expression
type Regex = Text

-- | Valid XML attribute name
type Name = Text

-- | Non-negative integer
newtype NonNegative = NonNegative Int
  deriving Num

-- | Valid XML token
newtype Token = Token String

-- | Some text and its associated XSD type
data AnySimpleType
  = AnySimpleType
  { _astValue :: Text
  , _astType :: XSDataType
  }

-- | 'annotation' element https://www.w3.org/TR/xmlschema-1/#element-annotation
data Annotation
  = Annotation
  { _annID :: Maybe ID
  , _annAttrs :: Attrs
  , _annContent :: [AnnotationContent]
  }

-- | Permitted content for 'annotation' elements
data AnnotationContent
  -- | 'appinfo' element https://www.w3.org/TR/xmlschema-1/#element-appinfo
  = AppInfo
  { _aiURI :: Maybe URI
  , _aiAttrs :: Attrs
  , _aiContent :: [Element]
  }
  
  -- | 'documentation' element https://www.w3.org/TR/xmlschema-1/#element-documentation
  | Documentation
  { _dcURI :: Maybe URI
  , _dcLanguage :: Maybe Language
  , _dcAttrs :: Attrs
  , _dcContent :: [Element]
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
    { _cfID :: Maybe ID
    , _cfLength :: Length
    , _cfAttrs :: Attrs
    , _cfAnnotation :: Maybe Annotation
    }
  
  -- | 'minLength' element https://www.w3.org/TR/xmlschema-2/#element-minLength
  | CFMinLength
    { _cfID :: Maybe ID
    , _cfMinLength :: MinLength
    , _cfAttrs :: Attrs
    , _cfAnnotation :: Maybe Annotation
    }
  
  -- | 'maxLength' element https://www.w3.org/TR/xmlschema-2/#element-maxLength
  | CFMaxLength
    { _cfID :: Maybe ID
    , _cfMaxLength :: MaxLength
    , _cfAttrs :: Attrs
    , _cfAnnotation :: Maybe Annotation
    }

  -- | 'pattern' element https://www.w3.org/TR/xmlschema-2/#element-pattern
  | CFPattern
    { _cfID :: Maybe ID
    , _cfPattern :: Pattern
    , _cfAttrs :: Attrs
    , _cfAnnotation :: Maybe Annotation
    }
    
  -- | 'enumeration' element https://www.w3.org/TR/xmlschema-2/#element-enumeration
  | CFEnumeration
    { _cfID :: Maybe ID
    , _cfEnumeration :: Enumeration
    , _cfAttrs :: Attrs
    , _cfAnnotation :: Maybe Annotation
    }
    
  -- | 'whiteSpace' element https://www.w3.org/TR/xmlschema-2/#element-whiteSpace
  | CFWhiteSpace
    { _cfID :: Maybe ID
    , _cfWhiteSpace :: WhiteSpace
    , _cfAttrs :: Attrs
    , _cfAnnotation :: Maybe Annotation
    }
    
  -- | 'maxInclusive' element https://www.w3.org/TR/xmlschema-2/#element-maxInclusive
  -- | 'minInclusive' element https://www.w3.org/TR/xmlschema-2/#element-minInclusive
  -- | 'maxExclusive' element https://www.w3.org/TR/xmlschema-2/#element-maxInclusive
  -- | 'minExclusive' element https://www.w3.org/TR/xmlschema-2/#element-minInclusive
  -- | 'totalDigits' element https://www.w3.org/TR/xmlschema-2/#element-totalDigits
  -- | 'fractionDigits' element https://www.w3.org/TR/xmlschema-2/#element-fractionDigits

-- | Forms
data Form = Qualified | Unqualified

-- | 'include' element https://www.w3.org/TR/xmlschema-1/#element-include
data Include
  = Include
  { _incID :: Maybe ID
  , _incSchemaLocation :: Maybe URI
  , _incAttrs :: Attrs
  , _incAnnotation :: Maybe Annotation
  }
  
-- | 'import' element https://www.w3.org/TR/xmlschema-1/#element-import
data Import
  = Import
  { _impID :: Maybe ID
  , _impNamespace :: Maybe URI
  , _impSchemaLocation :: Maybe URI
  , _impAttrs :: Attrs
  , _impAnnotation :: Maybe Annotation
  }
  
-- | Permitted values when 'simpleType's 'final' attribute is a list
data STFFinal = STFList | STFUnion | STFRestriction

-- | Permitted values of 'simpleType's 'final' attribute
data STFinal = STAll | STList STFFinal

-- | 'simpleType' element https://www.w3.org/TR/xmlschema-1/#element-simpleType 
data SimpleType
  = SimpleType
  { _stID :: Maybe ID
  , _stName :: Maybe NCName
  , _stFinal :: Maybe STFinal
  , _stAnnotation :: Maybe Annotation
  }
  
data CTBlock = CTBExtension | CTBRestriction
data CTFinal = CTFExtension | CTFRestriction

-- | Enumeration for possible contents of 'complexType' element https://www.w3.org/TR/xmlschema-1/#element-complexType
data CTContent

  -- | Containing a 'simpleContent' element https://www.w3.org/TR/xmlschema-1/#element-simpleContent
  = CTSimpleContent
  { _ctscID :: Maybe ID
  , _ctscAttrs :: Attrs
  , _ctscAnnotation :: Maybe Annotation
  , _ctscContent :: Either SimpleRestriction SimpleExtension
  }

  -- | Containing a 'complexContent' element https://www.w3.org/TR/xmlschema-1/#element-complexContent
  | CTComplexContent
  { _ctccID :: Maybe ID
  , _ctccAttrs :: Attrs
  , _ctccMixed :: Maybe Bool
  , _ctccAnnotation :: Maybe Annotation
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
  { _ctID :: Maybe ID
  , _ctAbstract :: Maybe Bool
  , _ctBlock :: Maybe CTBlock
  , _ctFinal :: Maybe CTFinal
  , _ctMixed :: Maybe Bool
  , _ctName :: Maybe NCName
  , _ctAnnotation :: Maybe Annotation
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
  { _redID :: Maybe ID
  , _redSchemaLocation :: Maybe URI
  , _redAttrs :: Attrs
  , _redContent :: [Either Annotation RedefineContent]
  }
  
-- | 'notation' element https://www.w3.org/TR/xmlschema-1/#element-notation
data Notation
  = Notation
  { _notID :: Maybe ID
  , _notName :: Maybe NCName
  , _notPublic :: Maybe Token
  , _notSystem :: Maybe URI
  , _notAttrs :: Attrs
  , _notAnnotation :: Maybe Annotation
  }
  
-- | Permitted values for an 'attribute's 'use' attribute
data Use = Optional | Prohibited | Required

-- | 'attribute' element https://www.w3.org/TR/xmlschema-1/#element-attribute 
data Attribute
  = Attribute
  { _attID :: Maybe ID
  , _attDefault :: Maybe Text
  , _attFixed :: Maybe Text
  , _attForm :: Maybe Form
  , _attName :: Maybe NCName
  , _attRef :: Maybe QName
  , _attType :: Maybe QName
  , _attUse :: Maybe Use
  , _attAttrs :: Attrs
  , _attAnnotation :: Maybe Annotation
  , _attSimpleType :: Maybe SimpleType
  }

-- | 'attributeGroup' element https://www.w3.org/TR/xmlschema-1/#element-attributeGroup
data AttributeGroup
  = AttributeGroup
  { _agID :: Maybe ID
  , _agName :: Maybe NCName
  , _agRef :: Maybe QName
  , _agAttrs :: Attrs
  , _agAnnotation :: Maybe Annotation
  , _agAttributeSpec :: [Either Attribute AttributeGroup]
  , _agAnyAttribute :: Maybe AnyAttribute
  }

-- | Upper bound on occurrances of things
data Occurances = Unbounded | Bounded NonNegative

-- | 'element' element https://www.w3.org/TR/xmlschema-1/#element-element
data Element
  = Element
  { _elID :: Maybe ID
  , _elAbstract :: Maybe Bool
  , _elForm :: Maybe Form
  , _elMaxOccurs :: Maybe Occurances
  , _elMinOccurs :: Maybe NonNegative
  , _elName :: NCName
  , _elNillable :: Maybe Bool
  , _elTypeName :: Maybe QName
  , _elAnnotation :: Maybe Annotation
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
  , _elAnnotation = Nothing
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
  elAnnotation :: Lens' s (Maybe Annotation)
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
  { _aaID :: Maybe ID
  , _aaNamespace :: Maybe Namespace
  , _aaProcessContents :: Maybe ProcessContents
  , _aaAttrs :: Attrs
  , _aaAnnotation :: Maybe Annotation
  }

-- | 'attributeGroup' element within a 'restriction' element within a 'simpleContent'
-- | element https://www.w3.org/TR/xmlschema-1/#element-simpleContent..attributeGroup
data SimpleAttributeGroup
  = SimpleAttributeGroup
  { _sagID :: Maybe ID
  , _sagRef :: Maybe QName
  , _sagAttrs :: Attrs
  , _sagAnnotation :: Maybe Annotation
  }

-- | 'restriction' element within a 'simpleContent' element https://www.w3.org/TR/xmlschema-1/#element-simpleContent..restriction
data SimpleRestriction
  = SimpleRestriction
  { _srsBase :: Maybe QName
  , _srsID :: Maybe ID
  , _srsAttrs :: Attrs
  , _srsType :: Maybe SimpleType
  , _srsAnnotation :: Maybe Annotation
  , _srsConstraints :: [ConstraintFacet]
  , _srsAttributeSpec :: [Either Attribute SimpleAttributeGroup]
  , _srsAnyAttribute :: Maybe AnyAttribute
  }

-- | 'extension' element within a 'simpleContent' element https://www.w3.org/TR/xmlschema-1/#element-simpleContent..restriction
data SimpleExtension
  = SimpleExtension
  { _sexBase :: Maybe QName
  , _sexID :: Maybe ID
  , _sexAnnotation :: Maybe Annotation
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
  { _allID :: Maybe ID
  , _allMaxOccurs :: One
  , _allMinOccurs :: Either Zero One
  , _allAttrs :: Attrs
  , _allAnnotation :: Maybe Annotation
  , _allContent :: [Element]
  }

-- | 'any' element https://www.w3.org/TR/xmlschema-1/#element-all
data Any
  = Any
  { _anyID :: Maybe ID
  , _anyMaxOccurs :: Occurances
  , _anyMinOccurs :: NonNegative
  , _anyNamespace :: Maybe Namespace
  , _anyProcessContents :: ProcessContents
  , _anyAttrs :: Attrs
  , _anyAnnotation :: Maybe Annotation
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
  { _choiceID :: Maybe ID
  , _choiceMaxOccurs :: Occurances
  , _choiceMinOccurs :: NonNegative
  , _choiceAttrs :: Attrs
  , _choiceAnnotation :: Maybe Annotation
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
  { _sequenceID :: Maybe ID
  , _sequenceMaxOccurs :: Maybe Occurances
  , _sequenceMinOccurs :: Maybe NonNegative
  , _sequenceAttrs :: Attrs
  , _sequenceAnnotation :: Maybe Annotation
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
  { _grID :: Maybe ID
  , _grMaxOccurs :: Maybe Occurances
  , _grMinOccurs :: Maybe NonNegative
  , _grName :: Maybe NCName
  , _grRef :: Maybe QName
  , _grAttrs :: Attrs
  , _grAnnotation :: Maybe Annotation
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
  { _cexID :: Maybe ID
  , _cexBase :: Maybe QName
  , _cexAttrs :: Attrs
  , _cexAnnotation :: Maybe Annotation
  , _cexGroupDefinition :: Maybe CTGroupDefinition
  , _cexAttributeSpec :: [Either Attribute AttributeGroup]
  , _cexAnyAttribute :: Maybe AnyAttribute
  }

-- | 'restriction' element within a 'complexContent' element https://www.w3.org/TR/xmlschema-1/#element-complexContent..restriction
data ComplexRestriction
  = ComplexRestriction
  { _cerID :: Maybe ID
  , _cerBase :: Maybe QName
  , _cerAttrs :: Attrs
  , _cerAnnotation :: Maybe Annotation
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
