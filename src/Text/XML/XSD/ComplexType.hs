{-# language LambdaCase #-}

module Text.XML.XSD.ComplexType
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

import Prelude (Maybe(..))

import Text.XML.Attrs
import Text.XML.XSD.Internal.Lenses
import Text.XML.XSD.Internal.Types

mkComplexType :: CTContent -> ComplexType
mkComplexType content
  = ComplexType
  { _ctID = Nothing
  , _ctAbstract = Nothing
  , _ctBlock = Nothing
  , _ctFinal = Nothing
  , _ctMixed = Nothing
  , _ctName = Nothing
  , _ctAttrs = emptyAttrs
  , _ctContent = content
  }
