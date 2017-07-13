{-# language TemplateHaskell #-}

module Text.XML.NonNegative
  ( NonNegative
  , isNonNegative
  , mkNonNegative
  , nn
  , _NonNegative
  )
  where

import Prelude

import Control.Lens (Prism', prism')
import Data.Char (isDigit)
import Data.Monoid
import Data.Text (Text)
import Language.Haskell.TH.Quote
import Numeric.Natural

import qualified Data.Text as T

type NonNegative = Natural

isNonNegative :: String -> Bool
isNonNegative = all isDigit

mkNonNegative :: String -> Maybe NonNegative
mkNonNegative a
  | isNonNegative a = Just $ read a
  | otherwise = Nothing

nn :: QuasiQuoter
nn =
  QuasiQuoter
  { quoteExp = \str ->
      case mkNonNegative str of
        Just n -> [| n |]
        Nothing -> fail $ str <> " is not a valid NonNegative"
  , quotePat = error "`nn` cannot be used as a pattern"
  , quoteType = error "`nn` cannot be used as a type"
  , quoteDec = error "`nn` cannot be used as a declaration"
  }

_NonNegative :: Prism' Text NonNegative
_NonNegative = prism' (T.pack . show) (mkNonNegative . T.unpack)
