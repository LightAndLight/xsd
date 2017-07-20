{-# language TemplateHaskell #-}

module Text.XML.XSD.SimpleType
  ( SimpleType(..)
  , AsSimpleType(..)
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

import Text.XML.XSD.Internal.Lenses
import Text.XML.XSD.Internal.Types
