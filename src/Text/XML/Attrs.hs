module Text.XML.Attrs where

import Control.Lens (Lens')
import Data.Map (Map)
import Data.Text (Text)

class HasAttrs s where
  attrs :: Lens' s (Map Text Text)
