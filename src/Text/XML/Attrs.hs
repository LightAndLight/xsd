{-# language TemplateHaskell #-}
module Text.XML.Attrs
  ( Attrs
  , HasAttrs(..)
  , emptyAttrs
  , (@$)
  , (@!)
  )
  where

import Prelude ((.))

import Control.Lens (Lens', makeLenses, (?~))
import Data.Map (Map)
import Data.Text (Text)

import qualified Data.Map as M

import Text.XML.QName

newtype Attrs = Attrs { _getAttrs :: Map QName Text}

makeLenses ''Attrs

emptyAttrs :: Attrs
emptyAttrs = Attrs M.empty

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
