{-# language TemplateHaskell #-}

module Text.XML.XSD.Validation.ModelGroup
  ( MGCompositor(..)
  , ModelGroup(..)
  -- * Lenses
  , mgName
  , mgTargetNamespace
  , mgCompositor
  , mgParticles
  )
  where

import Text.XML.XSD.Validation.Internal
