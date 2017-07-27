{-# language TemplateHaskell #-}

module Text.XML.XSD.Component.ModelGroup
  ( MGCompositor(..)
  , ModelGroup(..)
  -- * Lenses
  , mgName
  , mgTargetNamespace
  , mgCompositor
  , mgParticles
  )
  where

import Text.XML.XSD.Component.Internal
