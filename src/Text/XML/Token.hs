{-# language TemplateHaskell #-}

module Text.XML.Token
  ( Token
  , isToken
  , mkToken
  , tk
  , _Token
  )
  where

import Prelude

import Control.Lens (Prism', prism')
import Data.Monoid
import Data.Text (Text)
import Language.Haskell.TH.Quote

import qualified Data.Text as T

newtype Token = Token { _getToken :: Text } deriving (Eq, Show)

mkToken :: String -> Maybe Token
mkToken i
  | isToken i = Just . Token $ T.pack i
  | otherwise = Nothing

isToken :: String -> Bool
isToken [] = True
isToken (x:xs) =
  x `notElem` whiteSpace &&
  case xs of
    [] -> True
    y:_ -> (x == ' ') == (y /= ' ')
  where
    whiteSpace = ['\n', '\r', '\t']

tk :: QuasiQuoter
tk =
  QuasiQuoter
  { quoteExp = \str ->
      if isToken str
         then [| Token (T.pack s) |]
         else fail $ str <> " is not a valid Token"
  , quotePat = error "`tk` cannot be used as a pattern"
  , quoteType = error "`tk` cannot be used as a type"
  , quoteDec = error "`tk` cannot be used as a declaration"
  }

_Token :: Prism' Text Token
_Token = prism' _getToken (mkToken . T.unpack)
