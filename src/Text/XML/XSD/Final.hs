{-# language MultiParamTypeClasses #-}

module Text.XML.XSD.Final (AsFinal(..)) where

import Control.Lens (Prism')

class AsFinal s a where
  _Final :: Prism' s a
