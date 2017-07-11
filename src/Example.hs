{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}

module Example where

import Prelude

import Control.Lens hiding (element)
import Data.Text (Text)

import Text.XML.Attrs
import Text.XML.XSD.ComplexType
import Text.XML.XSD.Element
import Text.XML.XSD.Schema
import Text.XML.XSD.Sequence
import Text.XML.XSD.Types

(@$) :: HasAttrs o => (i -> o) -> (Name, Text) -> i -> o
(@$) f (name, value) = \i -> f i & attrs . at name ?~ value

(@!) :: HasAttrs o => o -> (Name, Text) -> o
(@!) f (name, value) = f & attrs . at name ?~ value

infixl 3 @!

test =
  schema
    @$ ("xmlns:xs", "http://www.w3.org/2001/XMLSchema")
    $
    [ schemaEntry & seBody .~
      [ _ComplexType # mkComplexType (
          _Sequence # mkSequence
          [ _Element' # (mkElement "to" & elTypeName ?~ "xs:string")
          , _Element' # (mkElement "from" & elTypeName ?~ "xs:string")
          , _Element' # (mkElement "heading" & elTypeName ?~ "xs:string")
          , _Element' # (mkElement "body" & elTypeName ?~ "xs:string")
          ])
      ]
    ]
