{-# language TemplateHaskell #-}
module Text.XML.Attrs
  ( Attrs
  , HasAttrs(..)
  , emptyAttrs
  , toNameTextMap
  , (@$)
  , (@!)
  )
  where

import Prelude ((.), Eq, Show)

import Control.Lens (Lens', makeLenses, (?~), (&), at)
import Data.Map (Map)
import Data.Text (Text)

import qualified Data.Map as M
import qualified Text.XML as XML

import Text.XML.XSD.Types.QName

newtype Attrs = Attrs { _getAttrs :: Map QName Text }
  deriving (Eq, Show)

makeLenses ''Attrs

emptyAttrs :: Attrs
emptyAttrs = Attrs M.empty

toNameTextMap :: Attrs -> Map XML.Name Text
toNameTextMap (Attrs a) = M.mapKeys qNameToName a

class HasAttrs s where
  attrs :: Lens' s (Map QName Text)

instance HasAttrs Attrs where
  attrs = getAttrs

(@$) :: HasAttrs o => (i -> o) -> (QName, Text) -> i -> o
(@$) f (name, value) = \i -> f i & attrs . at name ?~ value

infixl 3 @$

(@!) :: HasAttrs o => o -> (QName, Text) -> o
(@!) f (name, value) = f & attrs . at name ?~ value

infixl 3 @!
