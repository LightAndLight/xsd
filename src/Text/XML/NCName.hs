{-# language DeriveLift #-}
{-# language TemplateHaskell #-}

module Text.XML.NCName
  ( NCName
  , isNCName
  , mkNCName
  , nc
  )
  where

import Prelude

import Control.Applicative
import Data.Char (isDigit, isLetter, isAlphaNum)
import Data.Monoid
import Data.Text (Text)
import Language.Haskell.TH.Quote

import qualified Data.Text as T

-- | XML non-colonized name
newtype NCName = NCName Text deriving (Eq, Ord, Show)

isNCName :: String -> Bool
isNCName input =
  liftA2 (||) isLetter (== '_') (head input) &&
  all (\c -> or $ [isDigit, isAlphaNum, (`elem` ['.','-','_'])] <*> pure c) input

mkNCName :: String -> Maybe NCName
mkNCName input
  | isNCName input = Just . NCName $ T.pack input
  | otherwise = Nothing

nc :: QuasiQuoter
nc =
  QuasiQuoter
  { quoteExp = \str ->
      if isNCName str
      then [| NCName str |]
      else fail $ str <> " is not a valid NCName"
  , quotePat = error "`nc` cannot be used as a pattern"
  , quoteType = error "`nc` cannot be used as a type"
  , quoteDec = error "`nc` cannot be used as a declaration"
  }
