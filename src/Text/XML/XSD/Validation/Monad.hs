{-|
Module: Text.XML.XSD.Validation.Monad
Description: Schema validation monad
-}

{-# language GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Text.XML.XSD.Validation.Monad where

import Prelude

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)

import Text.XML.XSD.Types.NCName
import Text.XML.XSD.Types.QName
import Text.XML.XSD.Types.URI

import qualified Text.XML.XSD.Validation.Notation as V
import qualified Text.XML.XSD.Validation.SimpleType as V

import qualified Text.XML.XSD.XMLRep.SimpleType as XML

data PrimitiveType
  = TAnySimpleType
  | TString
  | TBoolean
  | TDecimal
  | TFloat
  | TDouble
  | TDuration
  | TDateTime
  | TTime
  | TDate
  | TGYearMonth
  | TGYear
  | TGMonthDay
  | TGDay
  | TGMonth
  | THexBinary
  | TBase64Binary
  | TAnyURI
  | TQName
  | TNOTATION
  deriving (Eq, Show)

type ValidationEnv = Maybe URI
data ValidationState
  = ValidationState
  { _simpleTypes :: Map QName V.SimpleType
  , _notations :: Map NCName V.Notation
  }

-- | Possible validation errors
data ValidationError
  -- | A simple type definition containing a restrction had no @type@
  -- | attribute and no valid contents
  = MissingBaseTypeDefinition XML.SimpleType

  -- | A simple type definition containing a list had no @itemType@
  -- | attribute and no valid contents
  | MissingItemTypeDefinition XML.SimpleType

  -- | A simple type definition containing a list had no @memberType@
  -- | attribute and no valid contents
  | MissingMemberTypeDefinition XML.SimpleType

  -- | The variety of a list's baseType was not union or atomic
  | BadItemTypeVariety XML.SimpleType V.STVariety

  -- | Some text could not be parsed as the expected type
  | IncorrectType Text PrimitiveType

  -- | A cycle was detected in a simple type definition with the given path
  | CyclicSimpleTypeDefinition XML.SimpleType (Set NCName)

  -- | The variety of a restriction did not match the variety of its base type.
  -- | Expected variety followed by actual variety
  | RestrictionVarietyMismatch XML.SimpleType V.STVariety V.STVariety

newtype ValidateT m a
  = ValidateT
  { runValidateT
    :: ExceptT ValidationError (StateT ValidationState (ReaderT ValidationEnv m)) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadState ValidationState
  , MonadReader ValidationEnv
  , MonadError ValidationError
  )

makeLenses ''ValidationState
