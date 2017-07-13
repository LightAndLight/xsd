{-# language OverloadedStrings #-}
module Text.XML.Boolean
  ( isBoolean
  , mkBoolean
  , _Boolean
  )
  where

import Prelude

import Data.Text (Text)
import Control.Lens (Prism', prism')

import qualified Data.Text as T

isBoolean :: String -> Bool
isBoolean = flip elem ["true", "false"]

mkBoolean :: String -> Maybe Bool
mkBoolean a =
  case a of
    "true" -> Just True
    "false" -> Just False
    _ -> Nothing

_Boolean :: Prism' Text Bool
_Boolean = prism' (\a -> if a then "true" else "false") (mkBoolean . T.unpack)
