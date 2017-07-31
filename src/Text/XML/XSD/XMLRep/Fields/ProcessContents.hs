{-|
Module: Text.XML.XSD.XMLRep.ProcessContents
Description: XSD @processContents@ attribute

Prism for the @processContents@ attribute, used in:

* https://www.w3.org/TR/xmlschema-1/#element-any
* https://www.w3.org/TR/xmlschema-1/#element-anyAttribute
-}

{-#
language

OverloadedStrings, LambdaCase
#-}
module Text.XML.XSD.XMLRep.Fields.ProcessContents
  ( ProcessContents(..)
  , _ProcessContents
  )
  where

import Prelude (Maybe(..), Eq, Show)

import Control.Lens
import Data.Text (Text)

-- | Permitted values of the 'processContents' attribute
data ProcessContents = PCLax | PCSkip | PCStrict
  deriving (Eq, Show)

_ProcessContents :: Prism' Text ProcessContents
_ProcessContents =
  prism'
    (\case
        PCLax -> "lax"
        PCSkip -> "skip"
        PCStrict -> "strict")

    (\case
        "lax" -> Just PCLax
        "skip" -> Just PCSkip
        "strict" -> Just PCStrict
        _ -> Nothing)
