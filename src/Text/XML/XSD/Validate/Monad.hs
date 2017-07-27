{-# language GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Text.XML.XSD.Validate.Monad where

import Prelude
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)

import qualified Text.XML as XML

import Text.XML.XSD.SimpleType
import Text.XML.XSD.Types.NCName

data ValidationError e
  = SchemaError e
  | ValidationError XML.Node e

data ValidationState
  = ValidationState
  { _vsTypeDefintions :: Map NCName SimpleType
  }
  
makeLenses ''ValidationState

newtype XSDValidationT e m a
  = Validation
  { runValidation
    :: ExceptT (ValidationError e) (StateT ValidationState m) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadState ValidationState
  , MonadError (ValidationError e)
  )

type XSDValidation e = XSDValidationT e Identity

validationError :: Monad m => XML.Node -> e -> XSDValidationT e m a
validationError n e = throwError $ ValidationError n e

schemaError :: Monad m => e -> XSDValidationT e m a
schemaError = throwError . SchemaError
