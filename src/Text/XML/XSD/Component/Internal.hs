{-# language TemplateHaskell #-}
module Text.XML.XSD.Component.Internal where

import Prelude (Bool, Maybe, Either)

import Control.Lens (makeLenses)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import Text.XML.XSD.Component.SimpleType
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
  , _ctFinal :: CTFinal
  , _ctAbstract :: Bool
  , _ctAttrUses :: [Attribute]
  , _ctAttrWildcard :: Maybe Wildcard
  , _ctContentType :: CTContentType
  , _ctProhibitedSubs :: CTProhibitedSubs
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
  { _mgName :: NCName
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
  
makeLenses ''ModelGroup
makeLenses ''ComplexType
makeLenses ''Attribute
makeLenses ''Particle
makeLenses ''Element
makeLenses ''IdentityConstraint
makeLenses ''Wildcard
makeLenses ''AttributeGroup
