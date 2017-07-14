module Text.XML.Regex
  ( Regex
  , isRegex
  , mkRegex
  , parseRegex
  , rx
  , _Regex
  )
  where

import Prelude

import Control.Lens (Prism', prism')
import Data.Attoparsec.Text (parseOnly)
import Data.Maybe
import Data.Text (Text)
import Language.Haskell.TH.Quote
import Text.Parser.Char
import Text.Parser.Combinators
import Text.RE.TDFA.String

import qualified Data.Text as T

type Regex = RE

isRegex :: Text -> Bool
isRegex = isJust . mkRegex

mkRegex :: Text -> Maybe Regex
mkRegex a =
  case parseOnly parseRegex a of
    Right r -> Just r
    _ -> Nothing

parseRegex :: (Monad m, CharParsing m) => m Regex
parseRegex = (some anyChar <* eof) >>= compileRegex

-- | Re-exported from Text.Regex to prevent clashes with 're' from 'Control.Lens'
rx :: QuasiQuoter
rx = re

_Regex :: Prism' Text Regex
_Regex = prism' (T.pack . reSource) mkRegex
