{-# language TemplateHaskell, QuasiQuotes #-}
module Text.XML.XSD.Component.Internal where

import Prelude (Bool(..), Maybe(..), Either(..), Eq, Show)

import Control.Lens (makeLenses)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import Text.XML.XSD.Component.ConstrainingFacet
import Text.XML.XSD.Component.FundamentalFacets
import Text.XML.XSD.Types.NCName
import Text.XML.XSD.Types.NonNegative
import Text.XML.XSD.Types.URI

data AttScope
  = ASGlobal
  | ASType ComplexType

data AttValueConstraint
  = AVDefault
  | AVFixed

data Attribute
  = Attribute
  { _attName :: NCName
  , _attTargetNamespace :: Maybe URI
  , _attTypeDefinition :: SimpleType
  , _attScope :: Maybe AttScope
  , _attValueConstraint :: Maybe (Text, AttValueConstraint)
  }
  
data CTDerivation
  = CTDExtension
  | CTDRestriction
  
data CTFinal
  = CTFExtension
  | CTFRestriction
  
data CTProhibitedSubs
  = CTPExtension
  | CTPRestriction
  
data CTContentType
  = CTCEmpty
  | CTCSimpleType SimpleType
  | CTCContentModelMixed Particle
  | CTCContentModelElementOnly Particle

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
  }
  
data ParticleMaxOccurs
  = PMUnbounded
  | PMBounded NonNegative

data ParticleTerm
  = PTModelGroup ModelGroup
  | PTWildcard Wildcard
  | PTElement Element

data Particle
  = Particle
  { _paMinOccurs :: NonNegative
  , _paMaxOccurs :: ParticleMaxOccurs
  , _paTerm :: ParticleTerm
  }
  
data ElScope
  = ESGlobal
  | ESType ComplexType
  
data ElValueConstraint
  = EVDefault
  | EVFixed

data ElSubsGroupExcl
  = ESGExtension
  | ESGRestriction

data ElDisallowedSubs
  = EDSubstitution
  | EDExtension
  | EDRestriction

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
  
data MGCompositor
  = MGCSequence
  | MGCChoice
  | MGAll
  
data ModelGroup
  = ModelGroup
  { _mgName :: Maybe NCName
  , _mgTargetNamespace :: Maybe URI
  , _mgCompositor :: MGCompositor
  , _mgParticles :: [Particle]
  }

data ICCategory
  = ICKey
  | ICKeyRef IdentityConstraint
  | ICUnique

data IdentityConstraint
  = IdentityConstraint
  { _icName :: NCName
  , _icTargetNamespace :: Maybe URI
  , _icCategory :: ICCategory
  , _icSelector :: Text
  , _icFields :: NonEmpty Text
  }

data WCNamespaceConstraint
  = WCNCAny
  | WCNCNot (Maybe URI)
  | WCNCMultiple [Maybe URI]

data WCProcessContents
  = WCPCSkip
  | WCPCLax
  | WCPCStrict

data Wildcard
  = Wildcard
  { _wcNamespaceConstraint :: WCNamespaceConstraint
  , _wcProcessContents :: WCProcessContents
  }

data AttributeGroup
  = AttributeGroup
  { _agName :: NCName
  , _agTargetNamespace :: Maybe URI
  , _agAttrUses :: [Attribute]
  , _agAttrWildcard :: Maybe Wildcard
  }
  
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
  
data AnySimpleType
  = AnySimpleType
  { _astName :: NCName
  , _astTargetNamespace :: URI
  , _astBaseType :: ComplexType
  }

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
