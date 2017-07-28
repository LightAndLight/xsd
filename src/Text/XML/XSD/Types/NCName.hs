{-# language TemplateHaskell #-}

module Text.XML.XSD.Types.NCName
  ( NCName
  , _getNCName
  , isNCName
  , mkNCName
  , parseNCName
  , nc
  , _NCName
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

import qualified Data.Text as T

-- | XML non-colonized name
newtype NCName
  = NCName { _getNCName :: Text }
  deriving (Eq, Ord, Show)

instance Lift NCName where
  lift (NCName n) = let n' = T.unpack n in [| NCName (T.pack n') |]

isNCName :: Text -> Bool
isNCName = isJust . mkNCName

mkNCName :: Text -> Maybe NCName
mkNCName input =
  case parseOnly parseNCName input of
    Right n -> Just n
    _ -> Nothing

parseNCName :: CharParsing m => m NCName
parseNCName = NCName . T.pack <$> liftA2 (:) (letter <|> char '_') (many ncNameChar)
  where
    ncNameChar =
      letter <|>
      digit <|>
      char '.' <|>
      char '-' <|>
      char '_'

nc :: QuasiQuoter
nc =
  QuasiQuoter
  { quoteExp = \str ->
      case mkNCName (T.pack str) of
        Just n -> [| n |]
        Nothing -> fail $ str <> " is not a valid NCName"
  , quotePat = error "`nc` cannot be used as a type"
  , quoteType = error "`nc` cannot be used as a type"
  , quoteDec = error "`nc` cannot be used as a declaration"
  }

_NCName :: Prism' Text NCName
_NCName = prism' _getNCName mkNCName
