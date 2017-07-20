{-#
language

LambdaCase, OverloadedStrings
#-}

module Text.XML.XSD.Namespace
  ( Namespace(..)
  , _Namespace
  , Locality(..)
  , _Locality
  )
  where

import Prelude
import Control.Lens
import Data.Text (Text)

import qualified Data.Text as T

import Text.XML.XSD.Types.URI
import Text.XML.XSD.Internal.Types

_Locality :: Prism' Text Locality
_Locality =
  prism'
    (\case
        TargetNamespace -> "##targetNamespace"
        Local -> "##local")
    (\case
       "##targetNamespace" -> Just TargetNamespace
       "##local" -> Just Local
       _ -> Nothing)

_Namespace :: Prism' Text Namespace
_Namespace = prism' nsToText textToNs
  where
    nsToText :: Namespace -> Text
    nsToText n =
      case n of
        NSAny -> "##any"
        NSOther -> "##other"
        NSList ns -> T.intercalate " " $ fmap f ns

    f (Left u) = review _URI u
    f (Right l) = review _Locality l
    
    textToNs :: Text -> Maybe Namespace 
    textToNs i =
      case i of
        "##any" -> Just NSAny
        "##other" -> Just NSOther
        _ -> Just . NSList $
          T.words i ^..
          folded .
          failing (_URI . re _Left) (_Locality . re _Right)
