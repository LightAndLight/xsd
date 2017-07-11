module Text.XML.XSD.Sequence
  ( mkSequence
  , AsSequence(..)
  , Sequence(..)
  , SequenceContent(..)
  )
  where

import Prelude (Maybe(..))

import qualified Data.Map as M

import Text.XML.XSD.Types

mkSequence :: [SequenceContent] -> Sequence
mkSequence content
  = Sequence
  { _sequenceID = Nothing
  , _sequenceMaxOccurs = Nothing
  , _sequenceMinOccurs = Nothing
  , _sequenceAttrs = M.empty
  , _sequenceAnnotation = Nothing
  , _sequenceContent = content
  }
