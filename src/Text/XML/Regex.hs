module Text.XML.Regex
  ( Regex
  , isRegex
  , mkRegex
  , rx
  , _Regex
  )
  where

import Prelude

import Control.Lens (Prism', prism')
import Data.Maybe
import Data.Text (Text)
import Language.Haskell.TH.Quote
import Text.RE.TDFA.String

import qualified Data.Text as T

type Regex = RE

isRegex :: String -> Bool
isRegex = isJust . compileRegex

mkRegex :: String -> Maybe Regex
mkRegex = compileRegex

_Regex :: Prism' Text Regex
_Regex = prism' (T.pack . reSource) (mkRegex . T.unpack)

-- | Re-exported from Text.Regex to prevent clashes with 're' from 'Control.Lens'
rx :: QuasiQuoter
rx = re
