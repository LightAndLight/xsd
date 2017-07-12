{-# language TemplateHaskell #-}

module Text.XML.XSD.SimpleType
  ( SimpleType(..)
  , STContent(..)
  , STFinal(..)
  , STFFinal(..)
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
