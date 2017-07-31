{-|
Module: Text.XML.XSD.Validation.Element
Description: @Simple Type Definition@ schema component

The @Simple Type Definition@ schema component

https://www.w3.org/TR/xmlschema-2/#Simple_Type_Definition_details
-}

module Text.XML.XSD.Validation.SimpleType
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

import Text.XML.XSD.Validation.Internal
