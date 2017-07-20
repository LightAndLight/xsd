{-# language DeriveLift #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}

module Text.XML.XSD.Types.QName
  ( QName
  , isQName
  , mkQName
  , parseQName
  , qn
  , nameToQName
  , qNameToName
  , _qnPrefix
  , _qnLocalPart
  , _QName
  )
  where

import Prelude

import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text (parseOnly)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Parser.Char
import Text.Parser.Combinators

import Text.XML.XSD.Types.NCName

import qualified Data.Text as T
import qualified Text.XML as XML

-- | Fully-qualified XML name
data QName
  = QName
  { _qnPrefix :: Maybe NCName
  , _qnLocalPart :: NCName
  }
  deriving (Eq, Lift, Ord, Show)

makeLenses ''QName

nameToQName :: XML.Name -> QName
nameToQName n =
  QName
  { _qnPrefix = fromJust . mkNCName <$> XML.namePrefix n
  , _qnLocalPart = fromJust . mkNCName $ XML.nameLocalName n
  }

-- TODO: Namespacing problems?
qNameToName :: QName -> XML.Name
qNameToName QName{..} =
  XML.Name
  { namePrefix = _getNCName <$> _qnPrefix
  , nameLocalName = _getNCName _qnLocalPart
  , nameNamespace = Nothing
  }
  
isQName :: Text -> Bool
isQName = isJust . mkQName

mkQName :: Text -> Maybe QName
mkQName str =
  case parseOnly parseQName str of
    Right q -> Just q
    _ -> Nothing

parseQName :: CharParsing m => m QName
parseQName = QName <$> optional (parseNCName <* char ':') <*> parseNCName <* eof

qn :: QuasiQuoter
qn =
  QuasiQuoter
  { quoteExp = \str ->
      case mkQName (T.pack str) of
        Just q -> [| q |]
        Nothing -> fail $ str <> " is not a valid QName"
  , quotePat = error "`qn` cannot be used as a pattern"
  , quoteType = error "`qn` cannot be used as a type"
  , quoteDec = error "`qn` cannot be used as a declaration"
  }

_QName :: Prism' Text QName
_QName = prism'
  (\s -> fromMaybe ""
    (s ^? qnPrefix . _Just . re _NCName) <>
    (s ^. qnLocalPart . re _NCName))
  mkQName
