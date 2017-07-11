{-# language TemplateHaskell #-}
module Text.XML.XSD.Element
  ( Element(..)
  , HasElement(..)
  , mkElement
  , mkElementS
  , mkElementC
  )
  where

import Prelude(Either(..), Maybe(..))

import Text.XML.NCName
import Text.XML.XSD.Types

mkElement' :: NCName -> Either SimpleType ComplexType -> Element
mkElement' name typeElem = (mkElement name) { _elTypeElement = Just typeElem }
  
mkElementS :: NCName -> SimpleType -> Element
mkElementS name content = mkElement' name (Left content)

mkElementC :: NCName -> ComplexType -> Element
mkElementC name content = mkElement' name (Right content)
