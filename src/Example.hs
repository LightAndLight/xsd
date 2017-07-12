{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module Example where

import Prelude

import Control.Lens hiding (element)
import Data.Text (Text)

import Text.XML.Attrs
import Text.XML.NCName
import Text.XML.QName
import Text.XML.XSD.ComplexType
import Text.XML.XSD.Element
import Text.XML.XSD.Schema
import Text.XML.XSD.Sequence
import Text.XML.XSD.Types

(@$) :: HasAttrs o => (i -> o) -> (QName, Text) -> i -> o
(@$) f (name, value) = \i -> f i & attrs . at name ?~ value

(@!) :: HasAttrs o => o -> (QName, Text) -> o
(@!) f (name, value) = f & attrs . at name ?~ value

infixl 3 @!

test =
  schema
    @$ ([qn|xmlns:xs|], "http://www.w3.org/2001/XMLSchema")
    $
    [ schemaEntry & seBody .~
      [ _ComplexType # mkComplexType (
          _Sequence # mkSequence
          [ _Element' # (mkElement [nc|to|] & elTypeName ?~ [qn|xs:string|])
          , _Element' # (mkElement [nc|from|] & elTypeName ?~ [qn|xs:string|])
          , _Element' # (mkElement [nc|heading|] & elTypeName ?~ [qn|xs:string|])
          , _Element' # (mkElement [nc|body|] & elTypeName ?~ [qn|xs:string|])
          ])
      ]
    ]
