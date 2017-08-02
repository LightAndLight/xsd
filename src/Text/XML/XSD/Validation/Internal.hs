{-# language TemplateHaskell, QuasiQuotes #-}
module Text.XML.XSD.Validation.Internal where

import Prelude

import Control.Lens (makeLenses)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import Text.XML.XSD.Validation.ConstrainingFacet
import Text.XML.XSD.Validation.FundamentalFacets
import Text.XML.XSD.Types.NCName
import Text.XML.XSD.Types.NonNegativeInteger
import Text.XML.XSD.Types.URI

data AttScope
  = ASGlobal
  | ASType ComplexType
  deriving (Eq, Show)

data AttValueConstraint
  = AVDefault
  | AVFixed
  deriving (Eq, Show)

data Attribute
  = Attribute
  { _attName :: NCName
  , _attTargetNamespace :: Maybe URI
  , _attTypeDefinition :: SimpleType
  , _attScope :: Maybe AttScope
  , _attValueConstraint :: Maybe (Text, AttValueConstraint)
  }
  deriving (Eq, Show)
  
data CTDerivation
  = CTDExtension
  | CTDRestriction
  deriving (Eq, Show)
  
data CTFinal
  = CTFExtension
  | CTFRestriction
  deriving (Eq, Show)
  
data CTProhibitedSubs
  = CTPExtension
  | CTPRestriction
  deriving (Eq, Show)
  
data CTContentType
  = CTCEmpty
  | CTCSimpleType SimpleType
  | CTCContentModelMixed Particle
  | CTCContentModelElementOnly Particle
  deriving (Eq, Show)

data ComplexType
  = ComplexType
  { _ctName :: Maybe NCName
  , _ctTargetNamespace :: Maybe URI
  , _ctBaseType :: Either SimpleType ComplexType
  , _ctDerivationMethod :: CTDerivation
  , _ctFinal :: [CTFinal]
  , _ctAbstract :: Bool
  , _ctAttrUses :: [Attribute]
  , _ctAttrWildcard :: Maybe Wildcard
  , _ctContentType :: CTContentType
  , _ctProhibitedSubs :: [CTProhibitedSubs]
  } deriving (Eq, Show)
  
data ParticleMaxOccurs
  = PMUnbounded
  | PMBounded NonNegativeInteger
  deriving (Eq, Show)

data ParticleTerm
  = PTModelGroup ModelGroup
  | PTWildcard Wildcard
  | PTElement Element
  deriving (Eq, Show)

data Particle
  = Particle
  { _paMinOccurs :: NonNegativeInteger
  , _paMaxOccurs :: ParticleMaxOccurs
  , _paTerm :: ParticleTerm
  }
  deriving (Eq, Show)
  
data ElScope
  = ESGlobal
  | ESType ComplexType
  deriving (Eq, Show)
  
data ElValueConstraint
  = EVDefault
  | EVFixed
  deriving (Eq, Show)

data ElSubsGroupExcl
  = ESGExtension
  | ESGRestriction
  deriving (Eq, Show)

data ElDisallowedSubs
  = EDSubstitution
  | EDExtension
  | EDRestriction
  deriving (Eq, Show)

data Element
  = Element
  { _elName :: NCName
  , _elTargetNamespace :: Maybe URI
  , _elTypeDefinition :: Either SimpleType ComplexType
  , _elScope :: Maybe ElScope
  , _elValueConstraint :: (Text, ElValueConstraint)
  , _elNillable :: Bool
  , _elConstraints :: [IdentityConstraint]
  , _elSubsGroupAff :: Maybe Element
  , _elSubsGroupExcl :: ElSubsGroupExcl
  , _elDisallowedSubs :: ElDisallowedSubs
  , _elAbstract :: Bool
  }
  deriving (Eq, Show)
  
data MGCompositor
  = MGCSequence
  | MGCChoice
  | MGAll
  deriving (Eq, Show)
  
data ModelGroup
  = ModelGroup
  { _mgName :: Maybe NCName
  , _mgTargetNamespace :: Maybe URI
  , _mgCompositor :: MGCompositor
  , _mgParticles :: [Particle]
  }
  deriving (Eq, Show)

data ICCategory
  = ICKey
  | ICKeyRef IdentityConstraint
  | ICUnique
  deriving (Eq, Show)

data IdentityConstraint
  = IdentityConstraint
  { _icName :: NCName
  , _icTargetNamespace :: Maybe URI
  , _icCategory :: ICCategory
  , _icSelector :: Text
  , _icFields :: NonEmpty Text
  }
  deriving (Eq, Show)

data WCNamespaceConstraint
  = WCNCAny
  | WCNCNot (Maybe URI)
  | WCNCMultiple [Maybe URI]
  deriving (Eq, Show)

data WCProcessContents
  = WCPCSkip
  | WCPCLax
  | WCPCStrict
  deriving (Eq, Show)

data Wildcard
  = Wildcard
  { _wcNamespaceConstraint :: WCNamespaceConstraint
  , _wcProcessContents :: WCProcessContents
  }
  deriving (Eq, Show)

data AttributeGroup
  = AttributeGroup
  { _agName :: NCName
  , _agTargetNamespace :: Maybe URI
  , _agAttrUses :: [Attribute]
  , _agAttrWildcard :: Maybe Wildcard
  }
  deriving (Eq, Show)
  
data STFinal
  = STFExtension
  | STFRestriction
  | STFList
  | STFUnion
  deriving (Eq, Show)

data STVariety
  = STVAtomic { _stvaPrimitive :: Either AnySimpleType SimpleType }
  | STVList { _stvlItemType :: SimpleType }
  | STVUnion { _stvuMemberTypes :: NonEmpty SimpleType }
  deriving (Eq, Show)

-- | Internal representation of a Simple Type Definition.
-- |
-- | Note: The 'Eq' and 'Ord' instances for this type only compare the @name@
-- | and @targetNamespace@ fields.
-- | See https://www.w3.org/TR/xmlschema-1#Simple_Type_Definition_details
data SimpleType
  = SimpleType
  { _stName :: Maybe NCName
  , _stTargetnamespace :: Maybe URI
  , _stBaseType :: Either AnySimpleType SimpleType
  , _stFacets :: [ConstrainingFacet]
  , _stFundamentalFacets :: FundamentalFacets
  , _stFinal :: [STFinal]
  , _stVariety :: STVariety
  }
  deriving Show

instance Eq SimpleType where
  a == b = (_stName a == _stName b) && (_stTargetnamespace a == _stTargetnamespace b)
  
instance Ord SimpleType where
  a <= b = (_stName a <= _stName b) && (_stTargetnamespace a <= _stTargetnamespace b) 
  
data AnySimpleType
  = AnySimpleType
  { _astName :: NCName
  , _astTargetNamespace :: URI
  , _astBaseType :: ComplexType
  }
  deriving (Eq, Show)

-- | Builtin 'anySimpleType' definition https://www.w3.org/TR/xmlschema-1/#simple-ur-type-itself
anySimpleType :: AnySimpleType
anySimpleType =
  AnySimpleType
  { _astName = [nc|anySimpleType|]
  , _astTargetNamespace = [uri|http://www.w3.org/2001/XMLSchema|]
  , _astBaseType = anyType
  }

-- | Builtin 'anyType' definition https://www.w3.org/TR/xmlschema-1/#ur-type-itself
anyType :: ComplexType
anyType =
  ComplexType
  { _ctName = Just [nc|anyType|]
  , _ctTargetNamespace = Just [uri|http://www.w3.org/2001/XMLSchema|]
  , _ctBaseType = Right anyType
  , _ctDerivationMethod = CTDRestriction
  , _ctFinal = []
  , _ctAbstract = False
  , _ctAttrUses = []
  , _ctAttrWildcard =
    Just
    Wildcard
    { _wcNamespaceConstraint = WCNCAny
    , _wcProcessContents = WCPCLax
    }
  , _ctContentType =
    CTCContentModelMixed
    Particle
    { _paMinOccurs = [nn|1|]
    , _paMaxOccurs = PMBounded [nn|1|]
    , _paTerm =
      PTModelGroup
      ModelGroup
      { _mgName = Nothing
      , _mgTargetNamespace = Nothing
      , _mgCompositor = MGCSequence
      , _mgParticles =
        [ Particle
          { _paMinOccurs = [nn|1|]
          , _paMaxOccurs = PMUnbounded
          , _paTerm =
            PTWildcard
            Wildcard
            { _wcNamespaceConstraint = WCNCAny
            , _wcProcessContents = WCPCLax
            }
          }
        ]
      }
    }
  , _ctProhibitedSubs = []
  }

makeLenses ''AnySimpleType
makeLenses ''SimpleType
makeLenses ''ModelGroup
makeLenses ''ComplexType
makeLenses ''Attribute
makeLenses ''Particle
makeLenses ''Element
makeLenses ''IdentityConstraint
makeLenses ''Wildcard
makeLenses ''AttributeGroup
