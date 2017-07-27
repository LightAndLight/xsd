module Text.XML.XSD.Component.FundamentalFacet where

import Prelude (Bool)

data Ordering
  = None
  | Partial
  | Total

data Cardinality
  = Finite
  | CountablyInfinite

-- | Fundamental facets https://www.w3.org/TR/xmlschema-2/#rf-fund-facets
data FundamentalFacet
  = FFEqual
  | FFOrdered
  { _ffOrderedValue :: Ordering }
  | FFBounded
  { _ffBoundedValue :: Bool }
  | FFCardinality
  { _ffCardinalityValue :: Cardinality }
  | FFNumeric
  { _ffNumericValue :: Bool }
