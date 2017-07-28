{-#
language

GeneralizedNewtypeDeriving, RecordWildCards, QuasiQuotes,
TemplateHaskell, OverloadedStrings, FlexibleContexts
#-}
module Text.XML.XSD.Component where

import Prelude

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)

import qualified Data.Map as M

import qualified Text.XML.XSD.XMLRep.ComplexType as XML
import qualified Text.XML.XSD.XMLRep.ConstrainingFacet as XML
import qualified Text.XML.XSD.XMLRep.Schema as XML
import qualified Text.XML.XSD.XMLRep.SimpleType as XML

import qualified Text.XML.XSD.Component.Builtins as Comp
import qualified Text.XML.XSD.Component.ComplexType as Comp
import qualified Text.XML.XSD.Component.ConstrainingFacet as Comp
import qualified Text.XML.XSD.Component.FundamentalFacets as Comp
import qualified Text.XML.XSD.Component.Notation as Comp
import qualified Text.XML.XSD.Component.Schema as Comp
import qualified Text.XML.XSD.Component.SimpleType as Comp

import Text.XML.XSD.Types.Base64Binary
import Text.XML.XSD.Types.Boolean
import Text.XML.XSD.Types.Date
import Text.XML.XSD.Types.DateTime
import Text.XML.XSD.Types.Decimal
import Text.XML.XSD.Types.Double
import Text.XML.XSD.Types.Duration
import Text.XML.XSD.Types.Float
import Text.XML.XSD.Types.HexBinary
import Text.XML.XSD.Types.NCName
import Text.XML.XSD.Types.Time
import Text.XML.XSD.Types.QName
import Text.XML.XSD.Types.URI

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
  { _simpleTypes :: Map QName Comp.SimpleType
  , _notations :: Map NCName Comp.Notation
  }
data ValidationError
  = MissingBaseTypeDefinition XML.SimpleType
  | MissingItemTypeDefinition XML.SimpleType
  | MissingMemberTypeDefinition XML.SimpleType
  | BadItemTypeVariety XML.SimpleType Comp.STVariety
  | IncorrectType Text PrimitiveType

makeLenses ''ValidationState

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

refineSchema :: Monad m => XML.Schema -> ValidateT m Comp.Schema
refineSchema XML.Schema{..} = do
  types <- traverse schemaElementToType _schemaBody
  pure
    Comp.Schema
    { _schemaTypes = catMaybes types
    , _schemaAttrs = _
    , _schemaElements = _
    , _schemaAttrGroups = _
    , _schemaModelGroups = _
    , _schemaNotations = _
    }
  where
    schemaElementToType
      :: Monad m
      => XML.SchemaElement
      -> ValidateT m (Maybe (Either Comp.SimpleType Comp.ComplexType))
    schemaElementToType e =
      case e of
        XML.SESimpleType st -> do
          st' <- refineSimpleType st
          case Comp._stName st' of
            Nothing -> pure ()
            Just name -> simpleTypes %= (M.insert (name ^?! re _NCName . _QName) st')
          pure . Just . Left $ st'
        XML.SEComplexType ct -> Just . Right <$> refineComplexType ct
        _ -> pure Nothing

lookupSimpleType
  :: MonadState ValidationState m
  => Maybe QName
  -> m (Maybe Comp.SimpleType)
lookupSimpleType a = uses simpleTypes $ \m -> flip M.lookup m =<< a

refineSimpleType :: Monad m => XML.SimpleType -> ValidateT m Comp.SimpleType
refineSimpleType s@XML.SimpleType{..} = do
  targetNamespace <- ask
  baseType <- simpleTypeBaseType _stContent
  facets <- simpleTypeFacets _stContent baseType
  variety <- simpleTypeVariety _stContent baseType
  pure
    Comp.SimpleType
    { _stName = _stName
    , _stTargetnamespace = targetNamespace
    , _stBaseType = baseType
    , _stFacets = facets
    , _stFundamentalFacets = simpleTypeFundamentalFacets variety baseType
    , _stFinal = simpleTypeFinal _stFinal
    , _stVariety = variety
    }
  where
    simpleTypeBaseType content =
      case content of
        XML.STRestriction{..} -> do
          maybeBase <- lookupSimpleType _strsBase
          typeElement <- traverse refineSimpleType _strsTypeElement
          case maybeBase <|> typeElement of
            Just ty -> pure . Right $ ty
            Nothing -> throwError $ MissingBaseTypeDefinition s
        XML.STList{} -> pure . Left $ Comp.anySimpleType
        XML.STUnion{} -> pure . Left $ Comp.anySimpleType
        
    simpleTypeFacets content baseType =
      case content of
        XML.STRestriction{..}
          | Right ty <- baseType ->
            refineConstrainingFacets _strsConstraints ty
        _ -> pure []

    simpleTypeVariety content baseType =
      case content of
        XML.STRestriction{..} -> pure $ Comp.STVAtomic baseType
        XML.STList{..} -> do
          maybeItemType <- lookupSimpleType _stlsItemType
          typeElement <- traverse refineSimpleType _stlsTypeElement
          case maybeItemType <|> typeElement of
            Just ty ->
              case Comp._stVariety ty of
                Comp.STVAtomic _ -> pure $ Comp.STVList ty
                Comp.STVUnion _ -> pure $ Comp.STVList ty
                v -> throwError $ BadItemTypeVariety s v
            Nothing -> throwError $ MissingItemTypeDefinition s
        XML.STUnion{..} -> do
          maybeMemberTypes <-
            traverse lookupSimpleType $
            sequence _stunMemberTypes
          typeElements <-
            traverseOf (traverse.traverse) refineSimpleType _stunTypeElements
          case sequence maybeMemberTypes <|> typeElements of
            Just (ty:tys) -> pure $ Comp.STVUnion (ty :| tys)
            _ -> throwError $ MissingMemberTypeDefinition s
            
    simpleTypeFinal final =
      case final of
        Just XML.STAll -> [Comp.STFRestriction, Comp.STFList, Comp.STFUnion]
        Just (XML.STMultiple vals) -> nub $ f vals
        Nothing -> []
      where
        f :: [XML.STFFinal] -> [Comp.STFinal]
        f [] = []
        f (v:vs) =
          case v of
            XML.STFList -> Comp.STFList : f vs
            XML.STFUnion -> Comp.STFUnion : f vs
            XML.STFRestriction -> Comp.STFRestriction : f vs
            
    simpleTypeFundamentalFacets variety baseType =
      Comp.FundamentalFacets
      { _ffOrdered =
          case variety of
            Comp.STVAtomic _
              | Right ty <- baseType -> ty ^. Comp.stFundamentalFacets . Comp.ffOrdered
              | otherwise -> Comp.None
            Comp.STVList _ -> Comp.None
            Comp.STVUnion _ -> _
      , _ffBounded = _
      , _ffCardinality = _
      , _ffNumeric = _
      }

hasSimpleType :: Monad m => Either Comp.AnySimpleType Comp.SimpleType -> Text -> ValidateT m Text
hasSimpleType (Left Comp.AnySimpleType{..}) input = pure input
hasSimpleType (Right Comp.SimpleType{..}) input = do
  (pred, ty) <-
    case _stName ^? _Just . re _NCName of
      Just "anySimpleType" -> pure (const True, TAnySimpleType)
      Just "string" -> pure (const True, TString)
      Just "boolean" -> pure (isBoolean, TBoolean)
      Just "decimal" -> pure (isDecimal, TDecimal)
      Just "float" -> pure (isFloat, TFloat)
      Just "double" -> pure (isDouble, TDouble)
      Just "duration" -> pure (isDuration, TDuration)
      Just "dateTime" -> pure (isDateTime, TDateTime)
      Just "time" -> pure (isTime, TTime)
      Just "date" -> pure (isDate, TDate)
      Just "gYearMonth" -> pure (isGYearMonth, TGYearMonth)
      Just "gYear" -> pure (isGYear, TGYear)
      Just "gMonthDay" -> pure (isGMonthDay, TGMonthDay)
      Just "gDay" -> pure (isGDay, TGDay)
      Just "gMonth" -> pure (isGMonth, TGMonth)
      Just "hexBinary" -> pure (isHexBinary, THexBinary)
      Just "base64Binary" -> pure (isBase64Binary, TBase64Binary)
      Just "anyURI" -> pure (isURI, TAnyURI)
      Just "QName" -> pure (isQName, TQName)
      Just "NOTATION" -> do
        ns <- use notations
        let
          isNotation i =
            case mkNCName i of
              Nothing -> False
              Just n -> n `M.member` ns
        pure (isNotation, TNotation)
      _ -> (const True, undefined)
  unless (pred input) . throwError $ IncorrectType input ty
  hasSimpleType _stBaseType input
  traverse_ (facetsHold input) _stFacets
  pure input

refineConstrainingFacets
  :: Monad m
  => [XML.ConstrainingFacet]
  -> Comp.SimpleType
  -> ValidateT m [Comp.ConstrainingFacet]
refineConstrainingFacets cfs expected =
  let (enums, cfs') = go cfs
  in
    liftA2 (:) (combineEnums enums) (pure cfs')
  where
    combineEnums enums = do
      values <- traverse (hasSimpleType (Right expected) . XML._cfEnumerationValue) enums
      pure
        Comp.CFEnumeration { _cfEnumerationValue = values }
    
    go [] = ([], [])
    go (cf:cfs) = 
      case cf of
        XML.CFLength{..} ->
          let
            val =
              Comp.CFLength
              { _cfLengthValue = _cfLengthValue
              , _cfLengthFixed = fromMaybe False _cfLengthFixed
              }
          in second (val :) $ go cfs
        XML.CFMinLength{..} ->
          let
            val =
              Comp.CFMinLength
              { _cfMinLengthValue = _cfMinLengthValue
              , _cfMinLengthFixed = fromMaybe False _cfMinLengthFixed
              }
          in second (val :) $ go cfs
        XML.CFMaxLength{..} ->
          let
            val =
              Comp.CFMaxLength
              { _cfMaxLengthValue = _cfMaxLengthValue
              , _cfMaxLengthFixed = fromMaybe False _cfMaxLengthFixed
              }
          in second (val :) $ go cfs
        XML.CFPattern{..} -> second (Comp.CFPattern{..} :) $ go cfs
        XML.CFEnumeration{..} -> first (cf :) $ go cfs
        XML.CFWhiteSpace{..} ->
          let
            val =
              Comp.CFWhiteSpace
              { _cfWhiteSpaceValue =
                case _cfWhiteSpaceValue of
                  XML.Collapse -> Comp.WSCollapse
                  XML.Replace -> Comp.WSReplace
                  XML.Preserve -> Comp.WSPreserve
              , _cfWhiteSpaceFixed = fromMaybe False _cfWhiteSpaceFixed
              }
          in second (val :) $ go cfs

refineComplexType :: Monad m => XML.ComplexType -> ValidateT m Comp.ComplexType
refineComplexType = _
