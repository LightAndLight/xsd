{-# language RecordWildCards #-}
module Text.XML.XSD.Validate.SimpleType where

import Prelude

import Control.Applicative
import Control.Monad
import Control.Lens hiding (List)
import Data.Maybe

import qualified Data.Map as M
import qualified Text.XML as XML

import Text.XML.XSD.SimpleType
import Text.XML.XSD.Types.QName
import Text.XML.XSD.Validate.Monad

data Variety = Atomic | List | Union

data STValidationError
  = NoBaseType SimpleType
  | BaseTypeFinalRestriction SimpleType SimpleType

variety
  :: Monad m
  => SimpleType
  -> XSDValidationT STValidationError m Variety
variety st =
  case st ^. stContent of
    STRestriction{} -> variety =<< baseTypeDefinition st
    STList{} -> pure List
    STUnion{} -> pure Union

baseTypeDefinition
  :: Monad m
  => SimpleType
  -> XSDValidationT STValidationError m SimpleType
baseTypeDefinition s@SimpleType{..} =
  case _stContent of
    STRestriction{..} -> do
      maybeBase <- uses vsTypeDefintions $
        \m -> flip M.lookup m . _qnLocalPart =<< _strsBase
      case maybeBase <|> _strsTypeElement of
        Nothing -> schemaError $ NoBaseType s
        Just base -> pure base
    STList{} -> _
    STUnion{} -> _

finalContains :: STFFinal -> STFinal -> Bool
finalContains f fs =
  case fs of
    STAll -> True
    STMultiple fs' -> f `elem` fs'
    
checkSimpleType
  :: Monad m
  => SimpleType
  -> XSDValidationT STValidationError m ()
checkSimpleType s@SimpleType{..} = do
  base <- baseTypeDefinition s
  when (isJust $ finalContains STFRestriction <$> base ^. stFinal) .
    schemaError $ BaseTypeFinalRestriction s base
  _
  where
    detectCycle :: XSDValidationT e m ()
    detectCycle = _

validateSimpleType :: XML.Node -> SimpleType -> XSDValidationT e m ()
validateSimpleType = _
