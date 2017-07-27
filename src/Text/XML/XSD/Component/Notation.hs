{-# language TemplateHaskell #-}
module Text.XML.XSD.Component.Notation
  ( Notation(..)
  , NotationIdentifier(..)
  -- * Lenses
  , notName
  , notTargetNamespace
  , notIdentifier
  )
  where

import Prelude (Maybe)

import Control.Lens (makeLenses)
import Data.Text (Text)

import Text.XML.XSD.Types.NCName
import Text.XML.XSD.Types.URI

data NotationIdentifier
  = NISystem URI (Maybe Text)
  | NIPublic Text (Maybe URI)

data Notation
  = Notation
  { _notName :: NCName
  , _notTargetNamespace :: Maybe URI
  , _notIdentifier :: NotationIdentifier
  }

makeLenses ''Notation
