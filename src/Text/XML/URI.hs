{-# language TemplateHaskell #-}

module Text.XML.URI
  ( URI
  , isURI
  , mkURI
  , uri
  , _URI
  )
  where

import Prelude

import Control.Lens (Prism', prism')
import Data.Monoid
import Data.Text (Text)
import Language.Haskell.TH.Quote
import Network.URI

import qualified Data.Text as T

mkURI :: String -> Maybe URI
mkURI = parseURI

uri :: QuasiQuoter
uri =
  QuasiQuoter
  { quoteExp = \str ->
      case mkURI str of
        Just (URI s Nothing p q f) -> [| URI s Nothing p q f |]
        Just (URI s (Just (URIAuth u r po)) p q f)
          -> [| URI s (Just (URIAuth u r po)) p q f |]
        Nothing
          -> fail $ str <> " is not a valid URI"
  , quotePat = error "`uri` cannot be used as a pattern"
  , quoteType = error "`uri` cannot be used as a type"
  , quoteDec = error "`uri` cannot be used as a declaration"
  }

_URI :: Prism' Text URI
_URI = prism' (T.pack . ($ "") . uriToString id) (mkURI . T.unpack)
