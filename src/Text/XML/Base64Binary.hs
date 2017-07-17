{-# language TemplateHaskell #-}

module Text.XML.Base64Binary
  ( Base64Binary
  , isBase64Binary
  , mkBase64Binary
  , b64
  , _Base64Binary
  )
  where

import Prelude

import Control.Applicative
import Control.Lens (Prism', prism')
import Data.Attoparsec.Text (parseOnly)
import Data.ByteString (ByteString)
import Data.Functor
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Word
import Language.Haskell.TH.Quote
import Text.Parser.Char
import Text.Parser.Combinators

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

type Base64Binary = ByteString

isBase64Binary :: Text -> Bool
isBase64Binary = isJust . mkBase64Binary

mkBase64Binary :: Text -> Maybe Base64Binary
mkBase64Binary i =
  case B64.decode . BC.pack . T.unpack $ i of
    Right b -> Just b
    _ -> Nothing

b64 :: QuasiQuoter
b64 =
  QuasiQuoter
  { quoteExp = \str ->
      case mkBase64Binary (T.pack str) of
        Just h -> let h' = B.unpack h in [| B.pack h' |]
        Nothing -> fail $ str <> " is not a valid Base64Binary"
  , quotePat = error "`b64` cannot be used as a pattern"
  , quoteType = error "`b64` cannot be used as a type"
  , quoteDec = error "`b64` cannot be used as a declaration"
  }

_Base64Binary :: Prism' Text Base64Binary
_Base64Binary =
  prism'
  (T.pack . BC.unpack . B64.encode)
  mkBase64Binary
