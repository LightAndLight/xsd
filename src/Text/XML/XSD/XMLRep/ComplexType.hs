{-# LANGUAGE LambdaCase #-}

module Text.XML.XSD.XMLRep.ComplexType
  ( mkComplexType
  , AsComplexType(..)
  , ComplexType(..)
  , CTBlock(..)
  , CTFinal(..)
  , CTContent(..)
  , ComplexRestriction(..)
  , ComplexExtension(..)
  , CTGroupDefinition(..)
  )
  where

import Prelude (Maybe (..))

import Text.XML.Attrs
import Text.XML.XSD.Internal.Lenses
import Text.XML.XSD.Internal.Types

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
