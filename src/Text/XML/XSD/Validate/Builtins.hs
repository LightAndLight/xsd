{-# language QuasiQuotes #-}
module Text.XML.XSD.Validate.Builtins where

import Prelude

import Text.XML.Attrs
import Text.XML.XSD.ComplexType
import Text.XML.XSD.SimpleType
import Text.XML.XSD.Types.NCName
import Text.XML.XSD.Types.QName

anyType :: ComplexType
anyType =
  ComplexType
  { _ctID = Nothing
  , _ctAttrs = emptyAttrs
  , _ctName = Just [nc|anyType|]
  , _ctFinal = Nothing
  , _ctAbstract = Just False
  , _ctBlock = Nothing
  , _ctMixed = False
  , _ctContent =
    CTSimpleContent
    { _ctscID = Nothing
    , _ctscAttrs = emptyAttrs
    , _ctscContent = Left
      SimpleRestriction
      { _srsBase = Just [nc|anyType|]
      , _srsID = Nothing
      , _srsAttrs = emptyAttrs
      , _srsType = Nothing
      , _srsConstraints =
        [ CFMinO
          
        ]
      , _srsAttributeSpec :: [Either Attribute SimpleAttributeGroup]
      , _srsAnyAttribute :: Maybe AnyAttribute
      }
    }
  }

anySimpleType :: SimpleType
anySimpleType =
  SimpleType
  { _stID = Nothing
  , _stName = Just [nc|anySimpleType|]
  , _stAttrs = emptyAttrs
  , _stFinal = Nothing
  , _stContent =
    STRestriction
    { _stcID = Nothing
    , _stcAttrs = emptyAttrs
    , _strsBase = Just [qn|anyType|]
    , _strsTypeElement = Nothing
    , _strsConstraints = []
    }
  }
