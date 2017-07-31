{-|
Module: Text.XML.XSD.XMLRep.ConstrainingFacet
Description: XSD constraining facet elements 

XSD constraining facets:

* @length@
* @minLength@
* @maxLength@
* @pattern@
* @enumeration@
* @whiteSpace@
* @maxInclusive@
* @maxExclusive@
* @maxInclusive@
* @minExclusive@
* @totalDigits@
* @fractionDigits@

https://www.w3.org/TR/xmlschema-2/#rf-facets
-}

module Text.XML.XSD.XMLRep.ConstrainingFacet
  ( ConstrainingFacet(..)
  , WhiteSpaceSetting(..)
  -- * Lenses
  , cfAttrs
  )
  where

import Text.XML.XSD.XMLRep.Internal.Types
