{-|
Module: Text.XML.XSD.Validation.Element
Description: @Element Declaration@ schema component

The @Element Declaration@ schema component

https://www.w3.org/TR/xmlschema-2/#Element_Declaration_details
-}

module Text.XML.XSD.Validation.Element
  ( Element(..)
  , ElScope(..)
  , ElValueConstraint(..)
  , ElSubsGroupExcl(..)
  , ElDisallowedSubs(..)
  -- * Lenses
  , elName
  , elTargetNamespace
  , elTypeDefinition
  , elScope
  , elValueConstraint
  , elNillable
  , elConstraints
  , elSubsGroupAff
  , elSubsGroupExcl
  , elDisallowedSubs
  , elAbstract
  )
  where

import Text.XML.XSD.Validation.Internal
