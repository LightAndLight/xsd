{-# language TemplateHaskell #-}

module Text.XML.XSD.Types.URI
  ( URI.URI
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

import qualified Data.Text as T
import qualified Network.URI as URI

isURI :: Text -> Bool
isURI = URI.isURI . T.unpack

mkURI :: Text -> Maybe URI.URI
mkURI = URI.parseURI . T.unpack

uri :: QuasiQuoter
uri =
  QuasiQuoter
  { quoteExp = \str ->
      case mkURI (T.pack str) of
        Just (URI.URI s Nothing p q f) -> [| URI.URI s Nothing p q f |]
        Just (URI.URI s (Just (URI.URIAuth u r po)) p q f)
          -> [| URI.URI s (Just (URI.URIAuth u r po)) p q f |]
        Nothing
          -> fail $ str <> " is not a valid URI"
  , quotePat = error "`uri` cannot be used as a pattern"
  , quoteType = error "`uri` cannot be used as a type"
  , quoteDec = error "`uri` cannot be used as a declaration"
  }

_URI :: Prism' Text URI.URI
_URI = prism' (T.pack . ($ "") . URI.uriToString id) mkURI
