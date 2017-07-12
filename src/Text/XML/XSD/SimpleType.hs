{-# language TemplateHaskell #-}

module Text.XML.XSD.SimpleType
  ( SimpleType(..)
  -- ^ Lenses
  , stID
  , stName
  , stFinal
  , stContent
  )
  where

import Control.Lens (makeLenses)
import Text.XML.XSD.Types

makeLenses ''SimpleType
