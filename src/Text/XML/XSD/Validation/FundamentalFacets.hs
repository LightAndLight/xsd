{-|
Module: Text.XML.XSD.Validation.FundamentalFacets
Description: Fundamental facets

Fundamental facets as described by:

* https://www.w3.org/TR/xmlschema-2/#rf-fund-facets
* https://www.w3.org/TR/xmlschema-2/#app-fundamental-facets
-}

{-# language TemplateHaskell #-}
module Text.XML.XSD.Validation.FundamentalFacets where

import Prelude (Bool, Eq, Show)

import Control.Lens (makeLenses)

data Ordering
  = None
  | Partial
  | Total
  deriving (Eq, Show)

data Cardinality
  = Finite
  | CountablyInfinite
  deriving (Eq, Show)

-- | Fundamental facets https://www.w3.org/TR/xmlschema-2/#rf-fund-facets
data FundamentalFacets
  = FundamentalFacets
  { _ffOrdered :: Ordering
  , _ffBounded :: Bool
  , _ffCardinality :: Cardinality
  , _ffNumeric :: Bool
  } deriving (Eq, Show)

makeLenses ''FundamentalFacets
