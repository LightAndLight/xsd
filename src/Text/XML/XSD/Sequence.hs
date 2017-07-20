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
import Text.XML.XSD.Lens
import Text.XML.XSD.Types

mkSequence :: [SequenceContent] -> Sequence
mkSequence content
  = Sequence
  { _sequenceID = Nothing
  , _sequenceMaxOccurs = Nothing
  , _sequenceMinOccurs = Nothing
  , _sequenceAttrs = emptyAttrs
  , _sequenceContent = content
  }
