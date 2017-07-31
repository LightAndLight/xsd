{-|
Module: Text.XML.XSD.XMLRep.Element
Description: XSD @element@ element

The XSD @element@ element

https://www.w3.org/TR/xmlschema-1/#element-element
-}

{-# language TemplateHaskell #-}
module Text.XML.XSD.XMLRep.Element
  ( Element(..)
  , mkElement
  , mkElementS
  , mkElementC
  -- * Lenses
  , HasElement(..)
  , AsElement(..)
  )
  where

import Prelude

import Control.Lens

import Text.XML.Attrs
import Text.XML.XSD.Types.NCName
import Text.XML.XSD.XMLRep.Internal.Lenses
import Text.XML.XSD.XMLRep.Internal.Types

-- | Construct a minimal element with the given name 
mkElement :: NCName -> Namespaced Element
mkElement name
  = (,) Nothing
  Element
  { _elID = Nothing
  , _elAbstract = Nothing
  , _elForm = Nothing
  , _elMaxOccurs = Nothing
  , _elMinOccurs = Nothing
  , _elName = Just name
  , _elNillable = Nothing
  , _elTypeName = Nothing
  , _elTypeElement = Nothing
  , _elAttrs = emptyAttrs
  }

-- | Construct a minimal element with the given name and content type
mkElement' :: NCName -> Either SimpleType ComplexType -> Namespaced Element
mkElement' name typeElem = mkElement name & _2 . elTypeElement ?~ typeElem

-- | Construct a minimal element with the given name and "SimpleType" description
mkElementS :: NCName -> SimpleType -> Namespaced Element
mkElementS name content = mkElement' name (Left content)

-- | Construct a minimal element with the given name and "ComplexType" description
mkElementC :: NCName -> ComplexType -> Namespaced Element
mkElementC name content = mkElement' name (Right content)
