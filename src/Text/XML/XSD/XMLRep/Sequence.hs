{-|
Module: Text.XML.XSD.XMLRep.Sequence
Description: XSD @sequence@ element

The XSD @sequence@ element

https://www.w3.org/TR/xmlschema-1/#element-sequence
-}

{-# language LambdaCase #-}
module Text.XML.XSD.XMLRep.Sequence
  ( Sequence(..)
  , SequenceContent(..)
  , mkSequence
  , AsSequence(..)
  )
  where

import Prelude (Maybe(..))

import Text.XML.Attrs
import Text.XML.XSD.XMLRep.Internal.Lenses
import Text.XML.XSD.XMLRep.Internal.Types

-- | Construct a minimal "Sequence" element containing some "SequenceContent"s
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
