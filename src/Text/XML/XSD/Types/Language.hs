{-|
Module: Text.XML.XSD.Types.Language
Description: XSD @language@ type

The @language@ type.

https://www.w3.org/TR/xmlschema-2/#language
-}

{-# language OverloadedStrings, TemplateHaskell #-}
module Text.XML.XSD.Types.Language
  ( Language
  , isLanguage
  , mkLanguage
  , parseLanguage
  , lang
  , _Language
  )
  where

import Prelude

import Control.Applicative
import Control.Lens (Prism', prism')
import Data.Attoparsec.Text (parseOnly)
import Data.Maybe (isJust)
import Data.Monoid
import Data.Text (Text)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Parser.Char
import Text.Parser.Combinators

import qualified Data.Text as T

data Language
  = Language
  { _idPart :: Text
  , _tagParts :: [Text]
  } deriving (Eq, Show)

instance Lift Language where
  lift (Language a bs) =
    let
      a' = T.unpack a
      bs' = fmap T.unpack bs
    in [| Language (T.pack a') (fmap T.pack bs') |]
    

mkLanguage :: Text -> Maybe Language
mkLanguage str =
  case parseOnly parseLanguage str of
    Right l -> Just l
    _ -> Nothing

isLanguage :: Text -> Bool
isLanguage = isJust . mkLanguage

parseLanguage :: CharParsing m => m Language
parseLanguage =
  Language <$>
  (T.pack <$> upTo1 8 letter) <*>
  many (char '-' *> (T.pack <$> upTo1 8 alphaNum))
  where
    upTo1 :: CharParsing m => Int -> m a -> m [a]
    upTo1 1 p = pure <$> p
    upTo1 n p = liftA2 (:) (try p) (upTo1 (n-1) p) <|> pure []

lang :: QuasiQuoter
lang =
  QuasiQuoter
  { quoteExp = \str ->
      case mkLanguage (T.pack str) of
        Just l -> [| l |]
        Nothing -> fail $ str <> " is not a valid Language"
  , quotePat = error "`lang` cannot be used as a pattern"
  , quoteType = error "`lang` cannot be used as a type"
  , quoteDec = error "`lang` cannot be used as a declaration"
  }
  
_Language :: Prism' Text Language
_Language =
  prism'
  (\(Language a bs) -> a <>
    if null bs then "" else "-" <>
    T.intercalate "-" bs)
  mkLanguage
