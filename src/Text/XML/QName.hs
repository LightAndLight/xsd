{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}

module Text.XML.QName
  ( QName
  , isQName
  , mkQName
  , qn
  , nameToQName
  , _qnPrefix
  , _qnLocalPart
  )
  where

import Prelude

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Language.Haskell.TH.Quote
import Text.XML.NCName

import qualified Text.Text as T
import qualified Text.XML as XML

-- | Fully-qualified XML name
data QName
  = QName
  { _qnPrefix :: Maybe NCName
  , _qnLocalPart :: NCName
  }
  deriving (Eq, Ord, Show)

nameToQName :: XML.Name -> QName
nameToQName n =
  QName
  { _qnPrefix = fromJust . mkNCName . T.unpack $ XML.namePrefix n
  , _qnLocalPart = fromJust . mkNCName . T.unpack $ XML.nameLocalName n
  }
  
isQName :: String -> Bool
isQName = isJust . getQName

mkQName :: String -> Maybe QName
mkQName str = do
  (before, after) <- getQName str
  liftA2 QName (mkNCName <$> before) (mkNCName after)
  
-- This is needed internally because we don't have an instance for `Lift Text`
-- for use in the quasiquoter
getQName :: String -> Maybe (Maybe String, String)
getQName str =
  let (before, after) = break (== ':') str
  in case after of
    [] -> if isNCName before
      then Just (Nothing, before)
      else Nothing
    ':' : rest -> if isNCName before && isNCName rest
      then Just (Just before, rest)
      else Nothing
    _ -> Nothing

qn :: QuasiQuoter
qn =
  QuasiQuoter
  { quoteExp = \str ->
      case getQName str of
        Just (Nothing, r) -> [| QName Nothing $(quoteExp nc r) |]
        Just (Just l, r) -> [| QName (Just $(quoteExp nc l)) $(quoteExp nc r) |]
        _ -> fail $ str <> " is not a valid QName"
  , quotePat = error "`qn` cannot be used as a pattern"
  , quoteType = error "`qn` cannot be used as a type"
  , quoteDec = error "`qn` cannot be used as a declaration"
  }

