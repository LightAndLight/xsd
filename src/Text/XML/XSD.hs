{-# language OverloadedStrings #-}

module Text.XML.XSD where

import Prelude
import Text.XML.Lens as XML
import qualified Text.XML.XSD.XMLRep.Schema as XSD

documentToXSD :: XML.Document -> Maybe XSD.Schema
documentToXSD document =
  let
    schemas = document ^.. root . named "schema"
  in case schemas of
    [] -> Nothing
    (_:_:_) -> error "Multiple schemas present. Need to look this up"
    [schema] -> schema ^? XSD._Schema
