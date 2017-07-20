{-# language ExistentialQuantification #-}
{-# language FlexibleContexts #-}
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
import Data.Char
import Data.Text (Text)

import qualified Data.Text as T

import Text.XML.Attrs
import Text.XML.Base64Binary
import Text.XML.Boolean
import Text.XML.Date
import Text.XML.DateTime
import Text.XML.Decimal
import Text.XML.Double
import Text.XML.Float
import Text.XML.HexBinary
import Text.XML.ID
import Text.XML.NCName
import Text.XML.NonNegative
import Text.XML.QName
import Text.XML.Regex
import Text.XML.Time
import Text.XML.Token
import Text.XML.URI
import Text.XML.XSD.Form

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
  | PHexBinary
  | PBase64Binary
  | PAnyURI
  | PQName
  {-
  | PNOTATION
  | PGYearMonth
  | PGYear
  | PGMonthDay
  | PGDay
  | PGMonth
  -}

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
        PDouble -> isDouble
        PDateTime -> isDateTime
        PTime -> isTime
        PDate -> isDate
        PHexBinary -> isHexBinary
        PBase64Binary -> isBase64Binary
        PAnyURI -> isURI
        PQName -> isQName
        {-
        PGYearMonth -> _
        PGYear -> _
        PGMonthDay -> _
        PGDay -> _
        PGMonth -> _
        PNOTATION -> _
        -}
     in if test txt then Just (AnySimpleType txt ty) else Nothing

-- | Permitted 'whiteSpace' 'value's
data WhiteSpaceSetting = Collapse | Replace | Preserve deriving Show

_WhiteSpace :: Prism' Text WhiteSpaceSetting
_WhiteSpace =
  prism'
    (T.pack . fmap toLower . show)
    (\case
        "collapse" -> Just Collapse
        "replace" -> Just Replace
        "preservce" -> Just Preserve
        _ -> Nothing)

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
    , _cfEnumerationValue :: QName
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

-- | 'include' element https://www.w3.org/TR/xmlschema-1/#element-include
data Include
  = Include
  { _incID :: Maybe NCName
  , _incSchemaLocation :: URI
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

-- | Permitted values of 'simpleType's 'final' attribute
data STFinal = STAll | STMultiple [STFFinal]

-- | Enumeration for possible contents of 'simpleType' element 
-- | https://www.w3.org/TR/xmlschema-1/#element-simpleType
data STContent
  -- | Containing a 'restriction' element
  -- | https://www.w3.org/TR/xmlschema-1/#element-restriction
  = STRestriction
  { _stcID :: Maybe NCName
  , _stcAttrs :: Attrs
  , _strsBase :: Maybe QName
  , _strsTypeElement :: Maybe SimpleType
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
  , _stunMemberTypes :: Maybe [QName]
  , _stunTypeElements :: Maybe [SimpleType]
  }

stcID :: Lens' STContent (Maybe NCName)
stcID = lens _stcID (\s a -> s { _stcID = a})

instance HasID STContent where
  id' = stcID

stcAttrs :: Lens' STContent Attrs
stcAttrs = lens _stcAttrs (\s a -> s { _stcAttrs = a})

instance HasAttrs STContent where
  attrs = stcAttrs . attrs


-- | 'simpleType' element https://www.w3.org/TR/xmlschema-1/#element-simpleType
data SimpleType
  = SimpleType
  { _stID :: Maybe NCName
  , _stAttrs :: Attrs
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
  { _ctgcGroupDefinition :: Maybe CTGroupDefinition
  , _ctgcAttributeSpec :: [Either Attribute AttributeGroup]
  , _ctgcAnyAttribute :: Maybe AnyAttribute
  }

emptyCTGroupContent :: CTContent
emptyCTGroupContent =
  CTGroupContent
  { _ctgcGroupDefinition = Nothing
  , _ctgcAttributeSpec = []
  , _ctgcAnyAttribute = Nothing
  }

-- | 'complexType' element https://www.w3.org/TR/xmlschema-1/#element-complexType
data ComplexType
  = ComplexType
  { _ctID :: Maybe NCName
  , _ctAttrs :: Attrs
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
  , _redSchemaLocation :: URI
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
  , _elName :: Maybe NCName
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
  , _elName = Just name
  , _elNillable = Nothing
  , _elTypeName = Nothing
  , _elTypeElement = Nothing
  , _elAttrs = emptyAttrs
  }

-- | Permitted values of when 'namespace' attribute is a list
data Locality = TargetNamespace | Local

-- | Permitted values of the 'namespace' attribute
data Namespace = NSAny | NSOther | NSList [Either URI Locality]

-- | Permitted values of the 'processContents' attribute
data ProcessContents = PCLax | PCSkip | PCStrict

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

-- | 'extension' element within a 'simpleContent' element https://www.w3.org/TR/xmlschema-1/#element-simpleContent..extension
data SimpleExtension
  = SimpleExtension
  { _sexBase :: Maybe QName
  , _sexID :: Maybe NCName
  , _sexAttrs :: Attrs
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
  , _allMaxOccurs :: Maybe One
  , _allMinOccurs :: Maybe (Either Zero One)
  , _allAttrs :: Attrs
  , _allContent :: [Element]
  }

-- | 'any' element https://www.w3.org/TR/xmlschema-1/#element-any
data Any
  = Any
  { _anyID :: Maybe NCName
  , _anyMaxOccurs :: Maybe Occurances
  , _anyMinOccurs :: Maybe NonNegative
  , _anyNamespace :: Maybe Namespace
  , _anyProcessContents :: Maybe ProcessContents
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
  , _choiceMaxOccurs :: Maybe Occurances
  , _choiceMinOccurs :: Maybe NonNegative
  , _choiceAttrs :: Attrs
  , _choiceContent :: [ChoiceContent]
  }

-- | Permitted content of a 'sequence' element
data SequenceContent
  = SCElement Element
  | SCGroup Group
  | SCSequence Sequence
  | SCAny Any

-- | 'sequence' element https://www.w3.org/TR/xmlschema-1/#element-sequence
data Sequence
  = Sequence
  { _sequenceID :: Maybe NCName
  , _sequenceMaxOccurs :: Maybe Occurances
  , _sequenceMinOccurs :: Maybe NonNegative
  , _sequenceAttrs :: Attrs
  , _sequenceContent :: [SequenceContent]
  }

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
  { _crsID :: Maybe NCName
  , _crsBase :: Maybe QName
  , _crsAttrs :: Attrs
  , _crsGroupDefinition :: Maybe CTGroupDefinition
  , _crsAttributeSpec :: [Either Attribute AttributeGroup]
  , _crsAnyAttribute :: Maybe AnyAttribute
  }

makeClassy ''Element
makeLenses ''SimpleType
makeLenses ''SimpleRestriction
makeLenses ''SimpleExtension
makeLenses ''SimpleAttributeGroup
makeLenses ''ComplexType
makeLenses ''ComplexRestriction
makeLenses ''ComplexExtension
makeLenses ''AnyAttribute
makeLenses ''Group
makeLenses ''All
makeLenses ''Any
makeLenses ''Choice
makeLenses ''Sequence
makeLenses ''AttributeGroup
makeLenses ''Attribute
makeLenses ''Notation
makeLenses ''Import
makeLenses ''Include
makeLenses ''Redefine

instance HasAttrs SimpleType where
  attrs = stAttrs . attrs
  
instance HasAttrs SimpleRestriction where
  attrs = srsAttrs . attrs
  
instance HasAttrs SimpleExtension where
  attrs = sexAttrs . attrs
  
instance HasAttrs SimpleAttributeGroup where
  attrs = sagAttrs . attrs

instance HasAttrs ComplexType where
  attrs = ctAttrs . attrs
  
instance HasAttrs ComplexRestriction where
  attrs = crsAttrs . attrs
  
instance HasAttrs ComplexExtension where
  attrs = cexAttrs . attrs
  
instance HasAttrs AnyAttribute where
  attrs = aaAttrs . attrs
  
instance HasAttrs Group where
  attrs = grAttrs . attrs
  
instance HasAttrs All where
  attrs = allAttrs . attrs
  
instance HasAttrs Any where
  attrs = anyAttrs . attrs
  
instance HasAttrs Choice where
  attrs = choiceAttrs . attrs
  
instance HasAttrs Sequence where
  attrs = sequenceAttrs . attrs
  
instance HasAttrs AttributeGroup where
  attrs = agAttrs . attrs
  
instance HasAttrs Attribute where
  attrs = attAttrs . attrs

instance HasAttrs Element where
  attrs = elAttrs . attrs
  
instance HasAttrs Notation where
  attrs = notAttrs . attrs
  
instance HasAttrs Import where
  attrs = impAttrs . attrs
  
instance HasAttrs Include where
  attrs = incAttrs . attrs
  
instance HasAttrs Redefine where
  attrs = redAttrs . attrs
