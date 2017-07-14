{-# language TemplateHaskell #-}

module Text.XML.Token
  ( Token
  , isToken
  , mkToken
  , parseToken
  , tk
  , _Token
  )
  where

import Prelude

import Control.Applicative
import Control.Monad
import Control.Lens (Prism', prism')
import Data.Attoparsec.Text (parseOnly)
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Monoid
import Data.Text (Text)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Parser.Char
import Text.Parser.LookAhead
import Text.Parser.Combinators

import qualified Data.Text as T

newtype Token = Token { _getToken :: Text } deriving (Eq, Show)

instance Lift Token where
  lift (Token t) = let t' = T.unpack t in [| Token (T.pack t') |]

isToken :: Text -> Bool
isToken = isJust . mkToken

mkToken :: Text -> Maybe Token
mkToken i =
  case parseOnly parseToken i of
    Right t -> Just t
    _ -> Nothing

parseToken :: (Monad m, CharParsing m, LookAheadParsing m) => m Token
parseToken = Token . T.pack <$> do
  a <- notSpace
  b <- many charOrSingleSpace
  c <- (pure <$> try notSpace) <|> pure ""
  pure (a : b <> c)
  where
    notSpace = satisfy (not . isSpace)
    charOrSingleSpace = do
      a <- noneOf "\r\n\t"
      when (a == ' ') $ lookAhead (void (noneOf "\r\n\t ") <|> eof)
      pure a

tk :: QuasiQuoter
tk =
  QuasiQuoter
  { quoteExp = \str ->
      case mkToken (T.pack str) of
        Just t -> [| t |]
        Nothing -> fail $ str <> " is not a valid Token"
  , quotePat = error "`tk` cannot be used as a pattern"
  , quoteType = error "`tk` cannot be used as a type"
  , quoteDec = error "`tk` cannot be used as a declaration"
  }

_Token :: Prism' Text Token
_Token = prism' _getToken mkToken
