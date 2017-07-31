{-|
Module: Text.XML.XSD.XMLRep.Form
Description: XSD @form@ attribute

Prism for the @form@ attribute, used in:

* https://www.w3.org/TR/xmlschema-1/#element-attribute
* https://www.w3.org/TR/xmlschema-1/#element-element
-}

{-# language OverloadedStrings #-}
module Text.XML.XSD.XMLRep.Fields.Form where

import Prelude

import Control.Lens (Prism', prism')
import Data.Char (toUpper)
import Data.Text (Text)

import qualified Data.Text as T

data Form = Qualified | Unqualified deriving (Eq, Show)

-- | Prism for things that might represent a "Form"
class AsForm s where
  _Form :: Prism' s Form

instance AsForm Text where
  _Form = prism'
    (T.pack . fmap toUpper . show)
    (\input -> case T.map toUpper input of
        "UNQUALIFIED" -> Just Unqualified
        "QUALIFIED" -> Just Qualified
        _ -> Nothing)
