{-# language MultiParamTypeClasses #-}

module Text.XML.XSD.Block where

import Control.Lens (Prism')

class AsBlock s a where
  _Block :: Prism' s a
