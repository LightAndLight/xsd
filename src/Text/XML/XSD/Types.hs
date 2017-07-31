{-|
Module: Text.XML.XSD.Types
Description: XSD Datatypes

Haskell datatypes, parsers and prisms for built-in primitive and derived XSD
datatypes.

* https://www.w3.org/TR/xmlschema-2/#built-in-primitive-datatypes
* https://www.w3.org/TR/xmlschema-2/#built-in-derived

-}
module Text.XML.XSD.Types
  ( -- * Primitive Datatypes
    module Text.XML.XSD.Types.Base64Binary
  , module Text.XML.XSD.Types.Boolean
  , module Text.XML.XSD.Types.Date
  , module Text.XML.XSD.Types.DateTime
  , module Text.XML.XSD.Types.Decimal
  , module Text.XML.XSD.Types.Double
  , module Text.XML.XSD.Types.Duration
  , module Text.XML.XSD.Types.Float
  , module Text.XML.XSD.Types.HexBinary
  , module Text.XML.XSD.Types.NCName
  , module Text.XML.XSD.Types.QName
  , module Text.XML.XSD.Types.Regex
  , module Text.XML.XSD.Types.Time
  -- * Derived Datatypes
  , module Text.XML.XSD.Types.ID
  , module Text.XML.XSD.Types.Language
  , module Text.XML.XSD.Types.NonNegativeInteger
  , module Text.XML.XSD.Types.PositiveInteger
  , module Text.XML.XSD.Types.Token
  )
  where

import Text.XML.XSD.Types.Base64Binary
import Text.XML.XSD.Types.Boolean
import Text.XML.XSD.Types.Date
import Text.XML.XSD.Types.DateTime
import Text.XML.XSD.Types.Decimal
import Text.XML.XSD.Types.Double
import Text.XML.XSD.Types.Duration
import Text.XML.XSD.Types.Float
import Text.XML.XSD.Types.HexBinary
import Text.XML.XSD.Types.ID
import Text.XML.XSD.Types.Language
import Text.XML.XSD.Types.NCName
import Text.XML.XSD.Types.NonNegativeInteger
import Text.XML.XSD.Types.PositiveInteger
import Text.XML.XSD.Types.QName
import Text.XML.XSD.Types.Regex
import Text.XML.XSD.Types.Time
import Text.XML.XSD.Types.Token
