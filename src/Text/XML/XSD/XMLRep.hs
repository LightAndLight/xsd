{-|
Module: Text.XML.XSD.XMLRep
Description: XML representation of schemas

Haskell datatypes, lenses and prisms for XML Schema documents and their fields
-}

module Text.XML.XSD.XMLRep
  ( -- * Special fields
    module Text.XML.XSD.XMLRep.Fields.Block
  , module Text.XML.XSD.XMLRep.Fields.Final
  , module Text.XML.XSD.XMLRep.Fields.Form
  , module Text.XML.XSD.XMLRep.Fields.Namespace
  , module Text.XML.XSD.XMLRep.Fields.ProcessContents
    -- * XSD Elements
  , module Text.XML.XSD.XMLRep.Attribute
  , module Text.XML.XSD.XMLRep.AttributeGroup
  , module Text.XML.XSD.XMLRep.ComplexType
  , module Text.XML.XSD.XMLRep.ConstrainingFacet
  , module Text.XML.XSD.XMLRep.Element
  , module Text.XML.XSD.XMLRep.Group
  , module Text.XML.XSD.XMLRep.Notation
  , module Text.XML.XSD.XMLRep.Schema
  , module Text.XML.XSD.XMLRep.Sequence
  , module Text.XML.XSD.XMLRep.SimpleType
  )
  where

import Text.XML.XSD.XMLRep.Attribute
import Text.XML.XSD.XMLRep.AttributeGroup
import Text.XML.XSD.XMLRep.ComplexType
import Text.XML.XSD.XMLRep.ConstrainingFacet
import Text.XML.XSD.XMLRep.Element
import Text.XML.XSD.XMLRep.Fields.Block
import Text.XML.XSD.XMLRep.Fields.Final
import Text.XML.XSD.XMLRep.Fields.Form
import Text.XML.XSD.XMLRep.Fields.Namespace
import Text.XML.XSD.XMLRep.Fields.ProcessContents
import Text.XML.XSD.XMLRep.Group
import Text.XML.XSD.XMLRep.Notation
import Text.XML.XSD.XMLRep.Schema
import Text.XML.XSD.XMLRep.Sequence
import Text.XML.XSD.XMLRep.SimpleType
