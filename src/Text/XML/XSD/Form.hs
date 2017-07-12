{-# language OverloadedStrings #-}

module Text.XML.XSD.Form where

import Prelude

import Control.Lens (Prism', prism')
import Data.Char (toUpper)
import Data.Text (Text)

import qualified Data.Text as T

-- | Element/attribute forms
data Form = Qualified | Unqualified deriving (Eq, Show)

class AsForm s where
  _Form :: Prism' s Form

instance AsForm Text where
  _Form = prism'
    (T.pack . fmap toUpper . show)
    (\input -> case T.map toUpper input of
        "UNQUALIFIED" -> Just Unqualified
        "QUALIFIED" -> Just Qualified
        _ -> Nothing)
