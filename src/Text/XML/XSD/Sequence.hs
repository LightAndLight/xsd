{-# language LambdaCase #-}

module Text.XML.XSD.Sequence
  ( mkSequence
  , AsSequence(..)
  , Sequence(..)
  , SequenceContent(..)
  )
  where

import Prelude (Maybe(..))

import Text.XML.Attrs
import Text.XML.XSD.Internal.Lenses
import Text.XML.XSD.Internal.Types

mkSequence :: [SequenceContent] -> Namespaced Sequence
mkSequence content
  = (,) Nothing
  Sequence
  { _sequenceID = Nothing
  , _sequenceMaxOccurs = Nothing
  , _sequenceMinOccurs = Nothing
  , _sequenceAttrs = emptyAttrs
  , _sequenceContent = content
  }
