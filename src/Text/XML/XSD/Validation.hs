{-|
Module: Text.XML.XSD.Validation
Description: Schema validation

Validation of XML schemas, and validation of XML documents with respect to
schemas
-}

{-#
language

GeneralizedNewtypeDeriving, RecordWildCards, QuasiQuotes,
TemplateHaskell, OverloadedStrings, FlexibleContexts
#-}
module Text.XML.XSD.Validation
  ( ValidationError(..)
  , validateSchema
  , validateDocument
  )
  where

import Prelude

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Either (rights)
import Data.Foldable
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Map as M
import qualified Data.Set as S

import qualified Text.XML as XML (Document)
import qualified Text.XML.XSD.XMLRep.ComplexType as XML
import qualified Text.XML.XSD.XMLRep.ConstrainingFacet as XML
import qualified Text.XML.XSD.XMLRep.Schema as XML
import qualified Text.XML.XSD.XMLRep.SimpleType as XML

import qualified Text.XML.XSD.Validation.Builtins as V
import qualified Text.XML.XSD.Validation.ComplexType as V
import qualified Text.XML.XSD.Validation.ConstrainingFacet as V
import qualified Text.XML.XSD.Validation.FundamentalFacets as V
import qualified Text.XML.XSD.Validation.Notation as V
import qualified Text.XML.XSD.Validation.Schema as V
import qualified Text.XML.XSD.Validation.SimpleType as V

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

-- | Validate an XML schema
validateSchema :: XML.Schema -> Either ValidationError XML.Schema
validateSchema = _

-- | Validate an XML document according to an XML schema
validateDocument
  :: XML.Document
  -> XML.Schema
  -> Either ValidationError XML.Document
validateDocument = _

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
  = MissingBaseTypeDefinition XML.SimpleType
  | MissingItemTypeDefinition XML.SimpleType
  | MissingMemberTypeDefinition XML.SimpleType
  | BadItemTypeVariety XML.SimpleType V.STVariety
  | IncorrectType Text PrimitiveType
  | CyclicSimpleTypeDefinition XML.SimpleType (Set NCName)

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

refineSchema :: Monad m => XML.Schema -> ValidateT m V.Schema
refineSchema XML.Schema{..} = do
  types <- traverse schemaElementToType _schemaBody
  pure
    V.Schema
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
      -> ValidateT m (Maybe (Either V.SimpleType V.ComplexType))
    schemaElementToType e =
      case e of
        XML.SESimpleType st -> do
          st' <- refineSimpleType st
          case V._stName st' of
            Nothing -> pure ()
            Just name -> simpleTypes %= (M.insert (name ^?! re _NCName . _QName) st')
          pure . Just . Left $ st'
        XML.SEComplexType ct -> Just . Right <$> refineComplexType ct
        _ -> pure Nothing

lookupSimpleType
  :: MonadState ValidationState m
  => Maybe QName
  -> m (Maybe V.SimpleType)
lookupSimpleType a = uses simpleTypes $ \m -> flip M.lookup m =<< a

onSTBaseType
  :: Monad m
  => (V.SimpleType -> ValidateT m a)
  -> (V.SimpleType -> ValidateT m a)
  -> ([V.SimpleType] -> ValidateT m a)
  -> XML.SimpleType
  -> ValidateT m a
onSTBaseType restriction list union s@XML.SimpleType{..} =
  case _stContent of
    XML.STRestriction{..} -> do
      maybeBase <- lookupSimpleType _strsBase
      typeElement <- traverse refineSimpleType _strsTypeElement
      case maybeBase of
        Nothing ->
          maybe
            (throwError $ MissingBaseTypeDefinition s)
            restriction
            typeElement
        Just ty -> restriction ty
    XML.STList{..} -> do
      maybeBase <- lookupSimpleType _stlsItemType
      typeElement <- traverse refineSimpleType _stlsTypeElement
      case maybeBase of
        Nothing ->
          maybe
            (throwError $ MissingBaseTypeDefinition s)
            list
            typeElement
        Just ty -> restriction ty
    XML.STUnion{..} -> do
      maybeBases <- traverse lookupSimpleType $ sequence _stunMemberTypes
      typeElements <- sequence $ traverse refineSimpleType <$> _stunTypeElements
      case sequence maybeBases of
        Nothing ->
          maybe
            (throwError $ MissingBaseTypeDefinition s)
            union
            typeElements
        Just tys -> union tys

stAncestors :: V.SimpleType -> [V.SimpleType]
stAncestors = go []
  where
    go acc st =
      let
        base = st ^? V.stBaseType . _Right
        acc' = maybe acc (flip (:) acc) base
      in maybe acc' (go acc') base

refineSimpleType :: Monad m => XML.SimpleType -> ValidateT m V.SimpleType
refineSimpleType s@XML.SimpleType{..} = do
  targetNamespace <- ask
  detectCycles s
  baseType <- simpleTypeBaseType _stContent
  facets <- simpleTypeFacets _stContent baseType
  variety <- simpleTypeVariety _stContent baseType
  pure
    V.SimpleType
    { _stName = _stName
    , _stTargetnamespace = targetNamespace
    , _stBaseType = baseType
    , _stFacets = facets
    , _stFundamentalFacets = simpleTypeFundamentalFacets variety baseType
    , _stFinal = simpleTypeFinal _stFinal
    , _stVariety = variety
    }
  where
    detectCycles :: Monad m => XML.SimpleType -> ValidateT m ()
    detectCycles sty =
      onSTBaseType
        (\ty -> do
            let newBase = ty ^. V.stBaseType
            either (const $ pure ()) (loop $ S.fromList (ty ^.. V.stName . _Just)) newBase)
        (\ty -> do
            let newBase = ty ^. V.stBaseType
            either (const $ pure ()) (loop $ S.fromList (ty ^.. V.stName . _Just)) newBase)
        (\tys -> do
            let newBases = tys ^.. folded . V.stBaseType . _Right
            traverse_ (loop $ S.fromList (newBases ^.. folded . V.stName . _Just)) newBases)
        sty
      where
        loop :: Monad m => Set NCName -> V.SimpleType -> ValidateT m ()
        loop history V.SimpleType{..} =
          case _stBaseType of
            Left _ -> pure ()
            Right ty
              | Just name <- (ty ^? V.stName . _Just)
              , name `S.member` history -> 
                  throwError $
                  CyclicSimpleTypeDefinition sty (maybe history (flip S.insert history) (ty ^. V.stName))
              | otherwise -> loop (maybe history (flip S.insert history) (ty ^. V.stName)) ty
              
    simpleTypeBaseType content =
      case content of
        XML.STRestriction{..} -> do
          maybeBase <- lookupSimpleType _strsBase
          typeElement <- traverse refineSimpleType _strsTypeElement
          case maybeBase <|> typeElement of
            Just ty -> pure . Right $ ty
            Nothing -> throwError $ MissingBaseTypeDefinition s
        XML.STList{} -> pure . Left $ V.anySimpleType
        XML.STUnion{} -> pure . Left $ V.anySimpleType
        
    simpleTypeFacets content baseType =
      case content of
        XML.STRestriction{..}
          | Right ty <- baseType ->
            refineConstrainingFacets _strsConstraints ty
        _ -> pure []

    simpleTypeVariety content baseType =
      case content of
        XML.STRestriction{..} -> pure $ V.STVAtomic baseType
        XML.STList{..} -> do
          maybeItemType <- lookupSimpleType _stlsItemType
          typeElement <- traverse refineSimpleType _stlsTypeElement
          case maybeItemType <|> typeElement of
            Just ty ->
              case V._stVariety ty of
                V.STVAtomic _ -> pure $ V.STVList ty
                V.STVUnion _ -> pure $ V.STVList ty
                v -> throwError $ BadItemTypeVariety s v
            Nothing -> throwError $ MissingItemTypeDefinition s
        XML.STUnion{..} -> do
          maybeMemberTypes <-
            traverse lookupSimpleType $
            sequence _stunMemberTypes
          typeElements <-
            traverseOf (traverse.traverse) refineSimpleType _stunTypeElements
          case sequence maybeMemberTypes <|> typeElements of
            Just (ty:tys) -> pure $ V.STVUnion (ty :| tys)
            _ -> throwError $ MissingMemberTypeDefinition s
            
    simpleTypeFinal final =
      case final of
        Just XML.STAll -> [V.STFRestriction, V.STFList, V.STFUnion]
        Just (XML.STMultiple vals) -> nub $ f vals
        Nothing -> []
      where
        f :: [XML.STFFinal] -> [V.STFinal]
        f [] = []
        f (v:vs) =
          case v of
            XML.STFList -> V.STFList : f vs
            XML.STFUnion -> V.STFUnion : f vs
            XML.STFRestriction -> V.STFRestriction : f vs
            
    simpleTypeFundamentalFacets variety baseType =
      V.FundamentalFacets
      { _ffOrdered =
          case variety of
            V.STVAtomic _
              | Right ty <- baseType -> ty ^. V.stFundamentalFacets . V.ffOrdered
              | otherwise -> V.None
            V.STVList _ -> V.None
            V.STVUnion _ -> _
      , _ffBounded = _
      , _ffCardinality = _
      , _ffNumeric = _
      }

hasSimpleType :: Monad m => Either V.AnySimpleType V.SimpleType -> Text -> ValidateT m Text
hasSimpleType (Left V.AnySimpleType{..}) input = pure input
hasSimpleType (Right V.SimpleType{..}) input = do
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
  -> V.SimpleType
  -> ValidateT m [V.ConstrainingFacet]
refineConstrainingFacets cfs expected =
  let (enums, cfs') = go cfs
  in
    liftA2 (:) (combineEnums enums) (pure cfs')
  where
    combineEnums enums = do
      values <- traverse (hasSimpleType (Right expected) . XML._cfEnumerationValue) enums
      pure
        V.CFEnumeration { _cfEnumerationValue = values }
    
    go [] = ([], [])
    go (cf:cfs) = 
      case cf of
        XML.CFLength{..} ->
          let
            val =
              V.CFLength
              { _cfLengthValue = _cfLengthValue
              , _cfLengthFixed = fromMaybe False _cfLengthFixed
              }
          in second (val :) $ go cfs
        XML.CFMinLength{..} ->
          let
            val =
              V.CFMinLength
              { _cfMinLengthValue = _cfMinLengthValue
              , _cfMinLengthFixed = fromMaybe False _cfMinLengthFixed
              }
          in second (val :) $ go cfs
        XML.CFMaxLength{..} ->
          let
            val =
              V.CFMaxLength
              { _cfMaxLengthValue = _cfMaxLengthValue
              , _cfMaxLengthFixed = fromMaybe False _cfMaxLengthFixed
              }
          in second (val :) $ go cfs
        XML.CFPattern{..} -> second (V.CFPattern{..} :) $ go cfs
        XML.CFEnumeration{..} -> first (cf :) $ go cfs
        XML.CFWhiteSpace{..} ->
          let
            val =
              V.CFWhiteSpace
              { _cfWhiteSpaceValue =
                case _cfWhiteSpaceValue of
                  XML.Collapse -> V.WSCollapse
                  XML.Replace -> V.WSReplace
                  XML.Preserve -> V.WSPreserve
              , _cfWhiteSpaceFixed = fromMaybe False _cfWhiteSpaceFixed
              }
          in second (val :) $ go cfs

refineComplexType :: Monad m => XML.ComplexType -> ValidateT m V.ComplexType
refineComplexType = _
