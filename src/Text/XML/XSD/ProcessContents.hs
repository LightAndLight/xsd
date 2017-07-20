{-#
language

OverloadedStrings, LambdaCase
#-}

module Text.XML.XSD.ProcessContents
  ( ProcessContents(..)
  , _ProcessContents
  )
  where

import Prelude (Maybe(..))

import Control.Lens
import Data.Text (Text)

import Text.XML.XSD.Types

_ProcessContents :: Prism' Text ProcessContents
_ProcessContents =
  prism'
    (\case
        PCLax -> "lax"
        PCSkip -> "skip"
        PCStrict -> "strict")

    (\case
        "lax" -> Just PCLax
        "skip" -> Just PCSkip
        "strict" -> Just PCStrict
        _ -> Nothing)
