{-# language TemplateHaskell #-}
module Text.XML.XSD.Component.SimpleType
  ( SimpleType(..)
  , STFinal(..)
  , STVariety(..)
  -- * Lenses
  , stName
  , stTargetnamespace
  , stBaseType
  , stFacets
  , stFundamentalFacets
  , stFinal
  , stVariety
  )
  where

import Prelude (Maybe)

import Control.Lens (makeLenses)
import Data.List.NonEmpty (NonEmpty)
import Text.XML.XSD.Component.ConstrainingFacet
import Text.XML.XSD.Component.FundamentalFacet
import Text.XML.XSD.Types.NCName
import Text.XML.XSD.Types.URI

data STFinal
  = STFExtension
  | STFRestriction
  | STFList
  | STFUnion

data STVariety
  = STVAtomic { _stvaPrimitive :: SimpleType }
  | STVList { _stvlItemType :: SimpleType }
  | STVUnion { _stvuMemberTypes :: NonEmpty SimpleType }

data SimpleType
  = SimpleType
  { _stName :: Maybe NCName
  , _stTargetnamespace :: Maybe URI
  , _stBaseType :: SimpleType
  , _stFacets :: [ConstrainingFacet]
  , _stFundamentalFacets :: [FundamentalFacet]
  , _stFinal :: STFinal
  , _stVariety :: STVariety
  }

makeLenses ''SimpleType
