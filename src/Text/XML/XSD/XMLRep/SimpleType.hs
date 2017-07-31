{-|
Module: Text.XML.XSD.XMLRep.SimpleType
Description: XSD @simpleType@ element

The XSD @simpleType@ element

https://www.w3.org/TR/xmlschema-1/#element-simpleType
-}

{-# language TemplateHaskell #-}
module Text.XML.XSD.XMLRep.SimpleType
  ( SimpleType(..)
  , STContent(..)
  , STFinal(..)
  , STFFinal(..)
  -- ^ Lenses
  , AsSimpleType(..)
  , stID
  , stName
  , stFinal
  , stContent
  )
  where

import Text.XML.XSD.XMLRep.Internal.Lenses
import Text.XML.XSD.XMLRep.Internal.Types
