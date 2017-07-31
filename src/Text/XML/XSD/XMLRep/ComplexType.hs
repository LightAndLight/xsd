{-|
Module: Text.XML.XSD.XMLRep.ComplexType
Description: XSD @complexType@ element

The XSD @complexType@ element

https://www.w3.org/TR/xmlschema-1/#element-complexType
-}

{-# language LambdaCase #-}
module Text.XML.XSD.XMLRep.ComplexType
  ( ComplexType(..)
  , mkComplexType
  , AsComplexType(..)
  , CTBlock(..)
  , CTFinal(..)
  , CTContent(..)
  , ComplexRestriction(..)
  , ComplexExtension(..)
  , CTGroupDefinition(..)
  -- * Lenses
  , ctID
  , ctAttrs
  , ctAbstract
  , ctBlock
  , ctFinal
  , ctMixed
  , ctName
  , ctContent
  )
  where

import Prelude (Maybe (..))

import Text.XML.Attrs
import Text.XML.XSD.XMLRep.Internal.Lenses
import Text.XML.XSD.XMLRep.Internal.Types

-- | Create a minimal "ComplexType" with some content
mkComplexType :: CTContent -> Namespaced ComplexType
mkComplexType content
  = (,) Nothing
  ComplexType
  { _ctID = Nothing
  , _ctAbstract = Nothing
  , _ctBlock = Nothing
  , _ctFinal = Nothing
  , _ctMixed = Nothing
  , _ctName = Nothing
  , _ctAttrs = emptyAttrs
  , _ctContent = content
  }
