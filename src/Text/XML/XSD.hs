{-# language OverloadedStrings #-}

module Text.XML.XSD where

import Prelude

import Text.XML.Lens as XML

import Text.XML.Attrs as XSD
import Text.XML.Language
import Text.XML.NCName
import Text.XML.QName
import Text.XML.Token
import Text.XML.URI
import Text.XML.XSD.Block
import Text.XML.XSD.Final
import Text.XML.XSD.Form
import Text.XML.XSD.Schema (Schema(..))
import Text.XML.XSD.SimpleType

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.XML as XML
import qualified Text.XML.XSD.Schema as XSD

documentToXSD :: XML.Document -> Maybe XSD.Schema
documentToXSD document =
  let
    schemas = document ^.. root . named "schema"
  in case schemas of
    [] -> Nothing
    (_:_:_) -> error "Multiple schemas present. Need to look this up"
    [schema] ->
      let
        schemaElements =
          (\name -> schema ^.. entire . named name) <$> XSD.schemaElementTags
      -- TODO: Decide whether invalid field values should result in a failed
      -- schema creation
      in Just Schema
        { _schemaID = schema ^? attr "id" . _NCName
        , _schemaAttributeFormDefault =
            schema ^? attr "attributeFormDefault" . _Form
        , _schemaBlockDefault =
            schema ^? attr "blockDefault" . _Block
        , _schemaElementFormDefault =
            schema ^? attr "elementFormDefault" . _Form
        , _schemaFinalDefault =
            schema ^? attr "finalDefault" . _Final
        , _schemaTargetNamespace =
            schema ^? attr "targetNamespace" . _URI
        , _schemaVersion =
            schema ^? attr "version" . _Token
        , _schemaXMLLang =
            schema ^? attr "language" . _Language
        , _schemaAttrs =
            emptyAttrs & XSD.attrs .~ M.mapKeys nameToQName (schema ^. XML.attrs)
        , _schemaPrelude = _
        , _schemaBody = _
        }

getSimpleType :: XML.Element -> Maybe SimpleType
getSimpleType e
  | e ^. localName == "simpleType" =
      Just SimpleType
        { _stID = e ^? attr "id" . _NCName
        , _stName = e ^? attr "name" . _NCName
        , _stFinal = e ^? attr "final" . _Final
        , _stContent = _
        }
  | otherwise = Nothing
