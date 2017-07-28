{-# language QuasiQuotes, TemplateHaskell #-}
module Text.XML.XSD.Component.Builtins
  ( AnySimpleType(..)
  , anySimpleType
  , anyType
  -- * Lenses
  , astName
  , astTargetNamespace
  , astBaseType
  )
  where

import Text.XML.XSD.Component.Internal
