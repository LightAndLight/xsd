{-# language TemplateHaskell #-}
module Text.XML.XSD.XMLRep.Element
  ( Element(..)
  , HasElement(..)
  , AsElement(..)
  , mkElement
  , mkElementS
  , mkElementC
  )
  where

import Prelude

import Control.Lens

import Text.XML.Attrs
import Text.XML.XSD.Types.NCName
import Text.XML.XSD.Internal.Lenses
import Text.XML.XSD.Internal.Types

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

mkElement' :: NCName -> Either SimpleType ComplexType -> Namespaced Element
mkElement' name typeElem = mkElement name & _2 . elTypeElement ?~ typeElem
  
mkElementS :: NCName -> SimpleType -> Namespaced Element
mkElementS name content = mkElement' name (Left content)

mkElementC :: NCName -> ComplexType -> Namespaced Element
mkElementC name content = mkElement' name (Right content)
