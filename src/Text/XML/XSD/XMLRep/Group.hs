{-|
Module: Text.XML.XSD.XMLRep.Group
Description: XSD @group@ element

The XSD @group@ element

https://www.w3.org/TR/xmlschema-1/#element-group
-}

module Text.XML.XSD.XMLRep.Group
  ( Group(..)
  , GroupContent(..)
  -- * Lenses
  , AsGroup(..)
  , grID
  , grMaxOccurs
  , grMinOccurs
  , grName
  , grRef
  , grAttrs
  , grContent
  )
  where

import Text.XML.XSD.XMLRep.Internal.Lenses
import Text.XML.XSD.XMLRep.Internal.Types
