{-|
Module: Text.XML.XSD.Validation.Builtins
Description: Built-in type definitions

Predefined type definitions
-}

module Text.XML.XSD.Validation.Builtins
  ( AnySimpleType(..)
  , anySimpleType
  , anyType
  -- * Lenses
  , astName
  , astTargetNamespace
  , astBaseType
  )
  where

import Text.XML.XSD.Validation.Internal
