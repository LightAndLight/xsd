module Text.XML.XSD.Types.ID where

import Prelude (Maybe)

import Control.Lens (Lens')

import Text.XML.XSD.Types.NCName

class HasID s where
  id' :: Lens' s (Maybe NCName)
