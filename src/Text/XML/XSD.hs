{-# language OverloadedStrings #-}

module Text.XML.XSD where

import Prelude

import Text.XML.Lens
import Text.XML.NCName
import Text.XML.XSD.Form
import Text.XML.XSD.Schema (Schema(..))

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
            schema ^? named "attributeFormDefault" . _Form
        , _schemaBlockDefault = _
        , _schemaElementFormDefault =
            schema ^? named "elementFormDefault" . _Form
        , _schemaFinalDefault = _
        , _schemaTargetNamespace = _
        , _schemaVersion = _
        , _schemaXMLLang = _
        , _schemaAttrs = _
        , _schemaPrelude = _
        , _schemaBody = _
        }
