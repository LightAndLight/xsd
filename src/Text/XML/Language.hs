{-# language TemplateHaskell #-}

module Text.XML.Language
  ( Language
  , isLanguage
  , mkLanguage
  , lang
  , _Language
  )
  where

import Prelude

import Control.Lens (Prism', prism')
import Data.Functor.Identity
import Data.Monoid
import Data.Text (Text)
import Language.Haskell.TH.Quote
import Text.RE.TDFA.String

import qualified Data.Text as T

newtype Language = Language { _getLanguage :: Text } deriving (Eq, Show)

mkLanguage :: String -> Maybe Language
mkLanguage i
  | isLanguage i = Just . Language $ T.pack i
  | otherwise = Nothing

languageRegex :: RE
languageRegex = runIdentity $ compileRegex "[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*"

isLanguage :: String -> Bool
isLanguage str = matched (str ?=~ languageRegex)

lang :: QuasiQuoter
lang =
  QuasiQuoter
  { quoteExp = \str ->
      if isLanguage str
         then [| Language (T.pack s) |]
         else fail $ str <> " is not a valid Language"
  , quotePat = error "`lang` cannot be used as a pattern"
  , quoteType = error "`lang` cannot be used as a type"
  , quoteDec = error "`lang` cannot be used as a declaration"
  }
  
_Language :: Prism' Text Language
_Language = prism' _getLanguage (mkLanguage . T.unpack)
