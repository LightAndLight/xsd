{-|
Module: Text.XML.XSD.Validation.ComplexType
Description: @Complex Type Definition@ schema component

The @Complex Type Definition@ schema component

https://www.w3.org/TR/xmlschema-1/#Complex_Type_Definition_details
-}

module Text.XML.XSD.Validation.ComplexType
  ( ComplexType(..)
  , CTDerivation(..)
  , CTFinal(..)
  , CTProhibitedSubs(..)
  , CTContentType(..)
  -- * Lenses
  , ctName
  , ctTargetNamespace
  , ctBaseType
  , ctDerivationMethod
  , ctFinal
  , ctAbstract
  , ctAttrUses
  , ctAttrWildcard
  , ctContentType
  , ctProhibitedSubs
  )
  where

import Text.XML.XSD.Validation.Internal
