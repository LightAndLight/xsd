module Text.XML.ID where

import Control.Lens (Lens')

import Text.XML.XSD.Types

class HasID s where
  id' :: Lens' s ID
