module Text.XML.XSD.Validate
  ( module Text.XML.XSD.Validate.Monad
  )
  where

import Prelude
 
import Control.Monad.Except
import Control.Monad.State

import Text.XML.XSD.Schema
import Text.XML.XSD.Validate.Monad

import qualified Text.XML as XML

validate :: XML.Document -> Schema -> Either ValidationError ()
validate = _
