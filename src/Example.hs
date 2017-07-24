{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module Example where

import Prelude

import Control.Lens

import Text.XML.Attrs
import Text.XML.XSD.Types.NCName
import Text.XML.XSD.Types.QName
import Text.XML.XSD.Types.URI
import Text.XML.XSD.ComplexType
import Text.XML.XSD.Element
import Text.XML.XSD.Schema
import Text.XML.XSD.Sequence

test =
  schema
    [ _ComplexType # mkComplexType (
        _Sequence # mkSequence
        [ _Element # (mkElement [nc|to|] & elTypeName ?~ [qn|xs:string|])
        , _Element # (mkElement [nc|from|] & elTypeName ?~ [qn|xs:string|])
        , _Element # (mkElement [nc|heading|] & elTypeName ?~ [qn|xs:string|])
        , _Element # (mkElement [nc|body|] & elTypeName ?~ [qn|xs:string|])
        ])
    ]
    & schemaNamespace .~ (Just [nc|xs|], [uri|http://www.w3.org/2001/XMLSchema|])
