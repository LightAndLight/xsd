module Text.XML.ID where

import Prelude (Maybe)

import Control.Lens (Lens')

import Text.XML.NCName

class HasID s where
  id' :: Lens' s (Maybe NCName)
