{-#
language

ExistentialQuantification, LambdaCase, OverloadedStrings, QuasiQuotes, RankNTypes, RecordWildCards, MultiParamTypeClasses,
TypeSynonymInstances, FlexibleInstances
#-}

module Text.XML.XSD.Internal.Lenses where

import Prelude

import Control.Applicative
import Control.Monad
import Control.Lens hiding (Choice)
import Data.Bifunctor
import Data.Functor
import Data.Map (Map)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.XML as XML
import qualified Text.XML.Lens as XML

import Text.XML.Attrs
import Text.XML.XSD.Types.Boolean
import Text.XML.XSD.Types.NCName
import Text.XML.XSD.Types.NonNegative
import Text.XML.XSD.Types.QName
import Text.XML.XSD.Types.Regex
import Text.XML.XSD.Types.Token
import Text.XML.XSD.Types.URI
import Text.XML.XSD.Block
import Text.XML.XSD.Final
import Text.XML.XSD.Form
import Text.XML.XSD.Namespace
import Text.XML.XSD.ProcessContents
import Text.XML.XSD.Internal.Types

createAttrs :: [XML.Name] -> Map XML.Name Text -> Attrs
createAttrs reservedKeys elemAttrs =
  emptyAttrs & attrs .~ M.mapKeys nameToQName (foldr M.delete elemAttrs reservedKeys)

data TEContent a =
  forall b.
  TEContent
  { tecGetter :: ReifiedFold a b
  , tecTransform :: ReifiedFold (Namespaced b) XML.Element
  }

data ToElement a
  = ToElement
  { teName :: a -> XML.Name
  , teAttrs :: a -> [Maybe (QName, Text)]
  , teContents :: [TEContent a]
  }
  
toElement
  :: HasAttrs a
  => ToElement a
  -> Namespaced a
  -> XML.Element
toElement ToElement{..} (ns, a) =
  XML.Element
  { elementName = teName a & XML._nameNamespace .~ fmap (review _NCName) ns
  , elementAttributes =
      M.mapKeys qNameToName $
      (a ^. attrs) `M.union` M.fromList (teAttrs a ^.. folded . _Just)
  , elementNodes =
      a ^..
      foldMap
        (\TEContent{..} ->
           runFold tecGetter .
           to ((,) ns) .
           runFold tecTransform . re XML._Element)
      teContents
  }

_ConstrainingFacet :: Prism' XML.Element (Namespaced ConstrainingFacet)
_ConstrainingFacet = prism' constraintFacetToElement elementToConstrainingFacet
  where
    constraintFacetToElement :: (Maybe NCName, ConstrainingFacet) -> XML.Element
    constraintFacetToElement =
      toElement 
        ToElement
        { teName = 
            \case
              CFLength{} -> "length"
              CFMinLength{} -> "minLength"
              CFMaxLength{} -> "maxLength"
              CFPattern{} -> "pattern"
              CFEnumeration{} -> "enumeration"
              CFWhiteSpace{} -> "whiteSpace"
        , teAttrs =
            \case
              CFLength{..} ->
                [ Just ([qn|value|], _NonNegative # _cfLengthValue)
                , (,) [qn|fixed|] . (_Boolean #) <$> _cfLengthFixed
                ]
              CFMinLength{..} ->
                 [ Just ([qn|value|], _NonNegative # _cfMinLengthValue)
                 , (,) [qn|fixed|] . (_Boolean #) <$> _cfMinLengthFixed
                 ]
              CFMaxLength{..} ->
                [ Just ([qn|value|], _NonNegative # _cfMaxLengthValue)
                , (,) [qn|fixed|] . (_Boolean #) <$> _cfMaxLengthFixed
                ]
              CFPattern{..} ->
                [ Just ([qn|value|], _Regex # _cfPatternValue)
                ]
              CFEnumeration{..} ->
                [ Just ([qn|value|], _cfEnumerationValue)
                ]
              CFWhiteSpace{..} ->
                [ Just ([qn|value|], _WhiteSpace # _cfWhiteSpaceValue)
                , (,) [qn|fixed|] . (_Boolean #) <$> _cfWhiteSpaceFixed
                ]
        , teContents = []
        }

    elementToConstrainingFacet :: XML.Element -> Maybe (Maybe NCName, ConstrainingFacet)
    elementToConstrainingFacet XML.Element{..} = do
      let
        reservedKeys = ["id", "fixed", "value"]
        i = elementAttributes ^? at "id" . _Just . _NCName
        f = elementAttributes ^? at "fixed" . _Just . _Boolean
        att = createAttrs reservedKeys elementAttributes
        ns = elementName ^. XML._nameNamespace
      el <- case XML.nameLocalName elementName of
        "length" ->
          let v = elementAttributes ^? at "value" . _Just . _NonNegative
          in CFLength i <$> v <*> pure f <*> pure att

        "minLength" ->
          let v = elementAttributes ^? at "value" . _Just . _NonNegative
          in CFMinLength i <$> v <*> pure f <*> pure att

        "maxLength" ->
          let v = elementAttributes ^? at "value" . _Just . _NonNegative
          in CFMaxLength i <$> v <*> pure f <*> pure att

        "pattern" ->
          let v = elementAttributes ^? at "value" . _Just . _Regex
          in CFPattern i <$> v <*> pure att

        "enumeration" ->
          let v = elementAttributes ^? at "value" . _Just
          in CFEnumeration i <$> v <*> pure att

        "whiteSpace" -> 
          let v = elementAttributes ^? at "value" . _Just . _WhiteSpace
          in CFWhiteSpace i <$> v <*> pure f <*> pure att

        _ -> Nothing

      withNamespace ns el
  
_SimpleTypeContent :: Prism' XML.Element (Namespaced STContent)
_SimpleTypeContent = prism' stContentToElement elementToStContent
  where
    stContentToElement :: Namespaced STContent -> XML.Element
    stContentToElement stc =
      let te = case snd stc of
            STRestriction{} ->
              ToElement
              { teName = elementName
              , teAttrs = elementAttrs
              , teContents = 
                  [ TEContent (Fold $ to _strsTypeElement . _Just) (Fold $ re _SimpleType)
                  , TEContent (Fold $ to _strsConstraints . folded) (Fold $ re _ConstrainingFacet)
                  ]
              }
            STList{} ->
              ToElement
              { teName = elementName
              , teAttrs = elementAttrs
              , teContents =
                  [ TEContent (Fold $ to _stlsTypeElement . _Just) (Fold $ re _SimpleType) ]
              }
            STUnion{} ->
              ToElement
              { teName = elementName
              , teAttrs = elementAttrs
              , teContents =
                [ TEContent (Fold $ to _stunTypeElements . _Just . folded) (Fold $ re _SimpleType) ]
              }
      in toElement te stc
      where
        elementName :: STContent -> XML.Name
        elementName =
          \case
            STRestriction{} -> "restriction"
            STList{} -> "list"
            STUnion{} -> "union"

        elementAttrs :: STContent -> [Maybe (QName, Text)]
        elementAttrs =
          \case
            STRestriction{..} ->
              [(,) [qn|base|] . review _QName <$> _strsBase]
            STList{..} ->
              [(,) [qn|itemType|] . review _QName <$> _stlsItemType]
            STUnion{..} -> 
              [(,) [qn|memberTypes|] . T.unwords . fmap (review _QName) <$> _stunMemberTypes]

    elementToStContent :: XML.Element -> Maybe (Namespaced STContent)
    elementToStContent XML.Element{..} = do
      let
        reservedKeys = ["id"]
        i = elementAttributes ^? at "id" . _Just . _NCName
        ns = elementName ^. XML._nameNamespace
      el <- case XML.nameLocalName elementName of
        "restriction" ->
          let
            att = createAttrs ("base" : reservedKeys) elementAttributes
            b = elementAttributes ^? at "base" . _Just . _QName
            st =
              elementNodes ^..
              folded .
              XML._Element .
              _SimpleType . _2
            cfs = 
              elementNodes ^..
              folded .
              XML._Element .
              _ConstrainingFacet . _2
          in
          Just $ STRestriction i att b (st ^? _head) cfs

        "list" ->
          let
            att = createAttrs ("itemType" : reservedKeys) elementAttributes
            it = elementAttributes ^? at "itemType" . _Just . _QName
            st = elementNodes ^.. folded . XML._Element . _SimpleType . _2
          in
          Just $ STList i att it (st ^? _head)

        "union" ->
          let
            att = createAttrs ("memberTypes" : reservedKeys) elementAttributes
            mts =
              toListOf (_Just . to T.words . folded . _QName) <$>
              (elementAttributes ^? at "memberTypes") 
            sts = 
              elementNodes ^..
              folded .
              XML._Element .
              _SimpleType . _2
          in
          Just $ STUnion i att mts (if null sts then Nothing else Just sts)
        _ -> Nothing

      withNamespace ns el

class AsSimpleType s where
  _SimpleType :: Prism' s (Namespaced SimpleType)

instance AsSimpleType XML.Element where
  _SimpleType = prism' simpleTypeToElement elementToSimpleType
    where
      simpleTypeToElement :: Namespaced SimpleType -> XML.Element
      simpleTypeToElement =
        toElement
        ToElement
        { teName = const "simpleType"
        , teAttrs = \SimpleType{..} ->
            [ (,) [qn|name|] . review _NCName <$> _stName
            , (,) [qn|final|] . review _Final <$> _stFinal
            ]
        , teContents =
            [ TEContent (Fold stContent) (Fold $ re _SimpleTypeContent) ]
        }

      elementToSimpleType :: XML.Element -> Maybe (Namespaced SimpleType)
      elementToSimpleType XML.Element{..} =
        let
          reservedKeys = ["id", "final", "name"]
          _stAttrs = createAttrs reservedKeys elementAttributes
          _stID = elementAttributes ^? at "id" . _Just . _NCName
          _stFinal = elementAttributes ^? at "final" . _Just . _Final
          _stName = elementAttributes ^? at "name" . _Just . _NCName
          maybeStContent =
            (elementNodes ^.. folded . XML._Element . _SimpleTypeContent . _2) ^? _head
          ns = elementName ^. XML._nameNamespace
        in do
          _stContent <- maybeStContent
          el <- case XML.nameLocalName elementName of
            "simpleType" -> Just SimpleType{..}
            _ -> Nothing
          withNamespace ns el

_SimpleAttributeGroup :: Prism' XML.Element (Namespaced SimpleAttributeGroup)
_SimpleAttributeGroup = prism' sagToElement elementToSag
  where
    sagToElement :: Namespaced SimpleAttributeGroup -> XML.Element
    sagToElement = toElement
      ToElement
      { teName = const "attributeGroup"
      , teAttrs = \SimpleAttributeGroup{..} ->
          [ (,) [qn|ref|] . review _QName <$> _sagRef ]
      , teContents = []
      }

    elementToSag :: XML.Element -> Maybe (Namespaced SimpleAttributeGroup)
    elementToSag XML.Element{..} = do
      let
        ns = elementName ^. XML._nameNamespace
      el <- case XML.nameLocalName elementName of
        "attributeGroup" ->
          let
            reservedKeys = ["id", "ref"]
            _sagID = elementAttributes ^? at "id" . _Just . _NCName
            _sagRef = elementAttributes ^? at "ref" . _Just . _QName
            _sagAttrs = createAttrs reservedKeys elementAttributes
          in
          Just SimpleAttributeGroup{..}
        _ -> Nothing
      withNamespace ns el

_AnyAttribute :: Prism' XML.Element (Namespaced AnyAttribute)
_AnyAttribute = prism' aaToElement elementToAa
  where
    aaToElement :: Namespaced AnyAttribute -> XML.Element
    aaToElement = toElement
      ToElement
      { teName = const "anyAttribute"
      , teAttrs = \AnyAttribute{..} -> Just <$>
          [ ( [qn|namespace|]
            , review _Namespace $ fromMaybe NSAny _aaNamespace
            )
          , ( [qn|processContents|]
            , review _ProcessContents $ fromMaybe PCStrict _aaProcessContents
            )  
          ]
      , teContents = [] 
      }

    elementToAa :: XML.Element -> Maybe (Namespaced AnyAttribute)
    elementToAa XML.Element{..} = do
      let
        ns = elementName ^. XML._nameNamespace
      el <- case XML.nameLocalName elementName of
        "anyAttribute" ->
          let
            reservedKeys = ["id", "namespace", "processContents"]
            _aaID =
              elementAttributes ^? at "id" . _Just . _NCName
            _aaNamespace =
              elementAttributes ^? at "namespace" . _Just . _Namespace
            _aaProcessContents = 
              elementAttributes ^? at "processContents" . _Just . _ProcessContents
            _aaAttrs = createAttrs reservedKeys elementAttributes
          in
          Just AnyAttribute{..}
        _ -> Nothing
      withNamespace ns el

prodDist :: Iso' (Either (a, b) (a, c)) (a, Either b c)
prodDist =
  iso
    (\case
        Left (a,b) -> (a, Left b)
        Right (a,c) -> (a, Right c))
    (\case
        (a, Left b) -> Left (a,b)
        (a, Right c) -> Right (a, c))

_SimpleRestriction :: Prism' XML.Element (Namespaced SimpleRestriction)
_SimpleRestriction = prism' srToElement elementToSr
  where
    srToElement :: Namespaced SimpleRestriction -> XML.Element
    srToElement = toElement
      ToElement
      { teName = const "restriction"
      , teAttrs = \SimpleRestriction{..} ->
        [ (,) [qn|base|] . review _QName <$> _srsBase ]
      , teContents =
        [ TEContent (Fold $ srsType . _Just) (Fold $ re _SimpleType)
        , TEContent (Fold $ srsConstraints . folded) (Fold $ re _ConstrainingFacet)
        , TEContent
          (Fold $ srsAttributeSpec . folded)
          (Fold $ re prodDist . re (without _Attribute _SimpleAttributeGroup) . bothEither)
        , TEContent (Fold $ srsAnyAttribute . _Just) (Fold $ re _AnyAttribute)
        ]
      }

    elementToSr :: XML.Element -> Maybe (Namespaced SimpleRestriction)
    elementToSr XML.Element{..} = do
      let
        ns = elementName ^. XML._nameNamespace
      el <- case XML.nameLocalName elementName of
        "restriction" ->
          let
            reservedKeys = ["base", "id"]
            _srsBase = elementAttributes ^? at "base" . _Just . _QName
            _srsID = elementAttributes ^? at "id" . _Just . _NCName
            _srsAttrs = createAttrs reservedKeys elementAttributes
            _srsType = elementNodes ^? folded . XML._Element . _SimpleType . _2
            _srsConstraints =
              elementNodes ^.. folded . XML._Element . _ConstrainingFacet . _2
            _srsAttributeSpec =
              elementNodes ^..
              folded .
              XML._Element .
              failing
                (_Attribute . _2 . re _Left)
                (_SimpleAttributeGroup . _2 . re _Right)
            _srsAnyAttribute =
              elementNodes ^? folded . XML._Element . _AnyAttribute . _2
          in
          Just SimpleRestriction{..}
        _ -> Nothing
      withNamespace ns el

_SimpleExtension :: Prism' XML.Element (Namespaced SimpleExtension)
_SimpleExtension = prism' seToElement elementToSe
  where
    seToElement :: Namespaced SimpleExtension -> XML.Element
    seToElement = toElement
      ToElement
      { teName = const "extension"
      , teAttrs = \SimpleExtension{..} ->
          [ (,) [qn|base|] . review _QName <$> _sexBase ]
      , teContents =
        [ TEContent
          (Fold $ sexAttributeSpec . folded)
          (Fold $ re prodDist . re (without _Attribute _SimpleAttributeGroup) . bothEither)
        , TEContent (Fold $ sexAnyAttribute . _Just) (Fold $ re _AnyAttribute)
        ]
      }

    elementToSe :: XML.Element -> Maybe (Namespaced SimpleExtension)
    elementToSe XML.Element{..} = do
      let
        ns = elementName ^. XML._nameNamespace
      el <- case XML.nameLocalName elementName of
        "extension" ->
          let
            reservedKeys = ["base", "id"]
            _sexBase = elementAttributes ^? at "base" . _Just . _QName
            _sexID = elementAttributes ^? at "id" . _Just . _NCName
            _sexAttrs = createAttrs reservedKeys elementAttributes
            _sexAttributeSpec =
              elementNodes ^..
              folded .
              XML._Element .
              failing
                (_Attribute . _2 . re _Left)
                (_SimpleAttributeGroup . _2 . re _Right)
            _sexAnyAttribute =
              elementNodes ^? folded . XML._Element . _AnyAttribute . _2
          in
          Just SimpleExtension{..}
        _ -> Nothing
      withNamespace ns el

_CTGroupDefinition :: Prism' XML.Element (Namespaced CTGroupDefinition)
_CTGroupDefinition =
  prism'
    (\case
        (ns, CTGDGroup g) -> review _Group (ns, g)
        (ns, CTGDAll a) -> review _All (ns, a)
        (ns, CTGDChoice c) -> review _Choice (ns, c)
        (ns, CTGDSequence s) -> review _Sequence (ns, s))

    $ \e ->
        (second CTGDGroup <$> e ^? _Group) <|>
        (second CTGDAll <$> e ^? _All) <|>
        (second CTGDChoice <$> e ^? _Choice) <|>
        (second CTGDSequence <$> e ^? _Sequence)

_ComplexRestriction :: Prism' XML.Element (Namespaced ComplexRestriction)
_ComplexRestriction = prism' crToElement elementToCr
  where
    crToElement :: Namespaced ComplexRestriction -> XML.Element
    crToElement = toElement
      ToElement
      { teName = const "restriction"
      , teAttrs = \ComplexRestriction{..} ->
        [ (,) [qn|base|] . review _QName <$> _crsBase ]
      , teContents =
        [ TEContent (Fold $ crsGroupDefinition . _Just) (Fold $ re _CTGroupDefinition)
        , TEContent
          (Fold $ crsAttributeSpec . folded)
          (Fold $ re prodDist . re (without _Attribute _AttributeGroup) . bothEither)
        , TEContent (Fold $ crsAnyAttribute . _Just) (Fold $ re _AnyAttribute)
        ]
      }

    elementToCr :: XML.Element -> Maybe (Namespaced ComplexRestriction)
    elementToCr XML.Element{..} = do
      let
        ns = elementName ^. XML._nameNamespace
      el <- case XML.nameLocalName elementName of
        "restriction" ->
          let
            reservedKeys = ["id", "base"]
            _crsID =
              elementAttributes ^? at "id" . _Just . _NCName
            _crsBase =
              elementAttributes ^? at "base" . _Just . _QName
            _crsAttrs = createAttrs reservedKeys elementAttributes
            _crsGroupDefinition =
              elementNodes ^? folded . XML._Element . _CTGroupDefinition . _2
            _crsAttributeSpec =
              elementNodes ^..
              folded .
              XML._Element .
              failing (_Attribute . _2 . re _Left) (_AttributeGroup . _2 . re _Right)
            _crsAnyAttribute =
              elementNodes ^? folded . XML._Element . _AnyAttribute . _2
          in
          Just ComplexRestriction{..}
        _ -> Nothing
      withNamespace ns el

_ComplexExtension :: Prism' XML.Element (Namespaced ComplexExtension)
_ComplexExtension = prism' ce ec
  where
    ce :: Namespaced ComplexExtension -> XML.Element
    ce = toElement
      ToElement
      { teName = const "extension"
      , teAttrs = \ComplexExtension{..} ->
          [ (,) [qn|base|] . review _QName <$> _cexBase ]
      , teContents =
          [ TEContent (Fold $ cexGroupDefinition . _Just) (Fold $ re _CTGroupDefinition)
          , TEContent
            (Fold $ cexAttributeSpec . folded)
            (Fold $ re prodDist . re (without _Attribute _AttributeGroup) . bothEither)
          , TEContent (Fold $ cexAnyAttribute . _Just) (Fold $ re _AnyAttribute)
          ]
      }

    ec :: XML.Element -> Maybe (Namespaced ComplexExtension)
    ec XML.Element{..} = do
      let
        ns = elementName ^. XML._nameNamespace
      el <- case XML.nameLocalName elementName of
        "extension" ->
          let
            reservedKeys = ["id", "base"]
            _cexID =
              elementAttributes ^? at "id" . _Just . _NCName
            _cexBase =
              elementAttributes ^? at "base" . _Just . _QName
            _cexAttrs = createAttrs reservedKeys elementAttributes
            _cexGroupDefinition =
              elementNodes ^? folded . XML._Element . _CTGroupDefinition . _2
            _cexAttributeSpec =
              elementNodes ^..
              folded .
              XML._Element .
              failing (_Attribute . _2 . re _Left) (_AttributeGroup . _2 . re _Right)
            _cexAnyAttribute =
              elementNodes ^? folded . XML._Element . _AnyAttribute . _2
          in
          Just ComplexExtension{..}
        _ -> Nothing
      withNamespace ns el

bothEither :: Iso' (Either a a) a
bothEither = iso (either id id) Left

_CTContent :: Prism' [XML.Element] (Namespaced CTContent)
_CTContent = prism' ctContentToElements elementsToCtContent
  where
    ctContentToElements :: Namespaced CTContent -> [XML.Element]
    ctContentToElements (ns, c) =
      case c of
        CTSimpleContent{..} ->
          [ XML.Element
            { elementName = "simpleContent"
            , elementAttributes = toNameTextMap _ctscAttrs
            , elementNodes =
                [ case _ctscContent of
                    Left a -> review (XML._Element . _SimpleRestriction) (ns, a)
                    Right b -> review (XML._Element . _SimpleExtension) (ns, b)
                ]
            }
          ]
        CTComplexContent{..} ->
          [ XML.Element
            { elementName = "complexContent"
            , elementAttributes = toNameTextMap _ctccAttrs
            , elementNodes =
                [ case _ctccContent of
                    Left a -> review (XML._Element . _ComplexRestriction) (ns, a)
                    Right b -> review (XML._Element . _ComplexExtension) (ns, b)
                ]
            }
          ]
        CTGroupContent{..} ->
          (_ctgcGroupDefinition ^.. _Just . to ((,) ns) . re _CTGroupDefinition) <>
          (_ctgcAttributeSpec ^..
            folded .
            to ((,) ns) .
            re prodDist .
            re (without _Attribute _AttributeGroup) .
            bothEither) <>
          (_ctgcAnyAttribute ^.. _Just . to ((,) ns) . re _AnyAttribute)

    elementsToCtContent :: [XML.Element] -> Maybe (Namespaced CTContent)
    elementsToCtContent [] = Nothing
    elementsToCtContent es =
      let
        simpleContent = do
          XML.Element{..} <- 
            es ^? folded . filtered ((==) "simpleContent" . XML.elementName)
          let
            ns = elementName ^. XML._nameNamespace
            _ctscID = elementAttributes ^? at "id" . _Just . _NCName
            _ctscAttrs =
              createAttrs ["id"] elementAttributes
          _ctscContent <-
            elementNodes ^?
              folded .
              XML._Element .
              failing
                (_SimpleRestriction . _2 . re _Left)
                (_SimpleExtension . _2 . re _Right)
          withNamespace ns CTSimpleContent{..}
             
        complexContent = do
          XML.Element{..} <- 
            es ^? folded . filtered ((==) "complexContent" . XML.elementName)
          let
            ns = elementName ^. XML._nameNamespace
            _ctccID = elementAttributes ^? at "id" . _Just . _NCName
            _ctccMixed = elementAttributes ^? at "mixed" . _Just . _Boolean
            _ctccAttrs =
              createAttrs ["id", "mixed"] elementAttributes
          _ctccContent <-
            elementNodes ^?
              folded .
              XML._Element .
              failing
                (_ComplexRestriction . _2 . re _Left)
                (_ComplexExtension . _2 . re _Right)
          withNamespace ns CTComplexContent{..}
          
        groupContent =
          let
            ns = es ^. _head . XML.name . XML._nameNamespace
            _ctgcGroupDefinition =
              es ^? folded . _CTGroupDefinition . _2
            _ctgcAttributeSpec =
              es ^..
              folded .
              failing
                (_Attribute . _2 . re _Left)
                (_AttributeGroup . _2 . re _Right)
            _ctgcAnyAttribute =
              es ^? folded . _AnyAttribute . _2
          in withNamespace ns CTGroupContent{..}
      in simpleContent <|> complexContent <|> groupContent

class AsComplexType s where
  _ComplexType :: Prism' s (Namespaced ComplexType)

instance AsComplexType XML.Element where
  _ComplexType = prism' complexTypeToElement elementToComplexType
    where
      complexTypeToElement :: Namespaced ComplexType -> XML.Element
      complexTypeToElement =
        toElement ToElement
        { teName = const "complexType"
        , teAttrs = \ComplexType{..} ->
            [ (,) [qn|abstract|] . review _Boolean <$> _ctAbstract
            , (,) [qn|block|] . review _Block <$> _ctBlock
            , (,) [qn|final|] . review _Final <$> _ctFinal
            , (,) [qn|mixed|] . review _Boolean <$> _ctMixed
            , (,) [qn|name|] . review _NCName <$> _ctName
            ]
        , teContents =
          [ TEContent (Fold ctContent) (Fold $ re _CTContent . folded) ]
        }

      elementToComplexType :: XML.Element -> Maybe (Namespaced ComplexType)
      elementToComplexType XML.Element{..} = do
        let
          ns = elementName ^. XML._nameNamespace
        el <- case XML.nameLocalName elementName of
          "complexType" -> do
            let
              reservedKeys = ["id", "abstract", "block", "final", "mixed", "name"]
              _ctID =
                elementAttributes ^? at "id" . _Just . _NCName
              _ctAttrs =
                createAttrs reservedKeys elementAttributes
              _ctAbstract =
                elementAttributes ^? at "abstract" . _Just . _Boolean
              _ctBlock =
                elementAttributes ^? at "block" . _Just . _Block
              _ctFinal =
                elementAttributes ^? at "final" . _Just . _Final
              _ctMixed =
                elementAttributes ^? at "mixed" . _Just . _Boolean
              _ctName =
                elementAttributes ^? at "name" . _Just . _NCName
            _ctContent <-
              elementNodes ^? to (^.. folded . XML._Element) . _CTContent . _2
            Just ComplexType{..}
          _ -> Nothing
        withNamespace ns el

_GroupContent :: Prism' XML.Element (Namespaced GroupContent)
_GroupContent = prism' ge eg
  where
    ge :: Namespaced GroupContent -> XML.Element
    ge = \case
      (ns, GCAll a) -> review _All (ns, a)
      (ns, GCChoice c) -> review _Choice (ns, c)
      (ns, GCSequence s) -> review _Sequence (ns, s)

    eg :: XML.Element -> Maybe (Namespaced GroupContent)
    eg e =
      (second GCAll <$> e ^? _All) <|>
      (second GCChoice <$> e ^? _Choice) <|>
      (second GCSequence <$> e ^? _Sequence)

_Occurances :: Prism' Text Occurances
_Occurances =
  prism'
    (\case
        Unbounded -> "unbounded"
        Bounded n -> review _NonNegative n)

    (\case
        "unbounded" -> Just Unbounded
        a -> Bounded <$> (a ^? _NonNegative))

class AsGroup s where
  _Group :: Prism' s (Namespaced Group)

instance AsGroup XML.Element where
  _Group = prism' ge eg
    where
      ge :: Namespaced Group -> XML.Element
      ge = toElement
        ToElement
        { teName = const "group"
        , teAttrs = \Group{..} ->
          [ (,) [qn|maxOccurs|] . review _Occurances <$> _grMaxOccurs
          , (,) [qn|minOccurs|] . review _NonNegative <$> _grMinOccurs
          , (,) [qn|name|] . review _NCName <$> _grName
          , (,) [qn|ref|] . review _QName <$> _grRef
          ]
        , teContents =
          [ TEContent (Fold $ grContent . _Just) (Fold $ re _GroupContent) ] 
        }

      eg :: XML.Element -> Maybe (Namespaced Group)
      eg XML.Element{..} = do
        let
          ns = elementName ^. XML._nameNamespace
        el <- case XML.nameLocalName elementName of
          "group" ->
            let
              reservedKeys = ["id", "maxOccurs", "minOccurs", "name", "ref"]
              _grID = elementAttributes ^? at "id" . _Just . _NCName
              _grMaxOccurs =
                elementAttributes ^? at "maxOccurs" . _Just . _Occurances
              _grMinOccurs =
                elementAttributes ^? at "minOccurs" . _Just . _NonNegative
              _grName =
                elementAttributes ^? at "name" . _Just . _NCName
              _grRef =
                elementAttributes ^? at "ref" . _Just . _QName
              _grAttrs =
                createAttrs reservedKeys elementAttributes
              _grContent =
                elementNodes ^? folded . XML._Element . _GroupContent . _2
            in
            Just Group{..}
          _ -> Nothing
        withNamespace ns el

_Zero :: Prism' Text Zero
_Zero =
  prism'
    (const "0")
    (\case
        "0" -> Just Zero
        _ -> Nothing)
    
_One :: Prism' Text One
_One =
  prism'
    (const "1")
    (\case
        "1" -> Just One
        _ -> Nothing)
  
class AsAll s where
  _All :: Prism' s (Namespaced All)

instance AsAll XML.Element where
  _All = prism' ae ea
    where
      ae :: Namespaced All -> XML.Element
      ae = toElement
        ToElement
        { teName = const "all"
        , teAttrs = \All{..} ->
          [ (,) [qn|id|] . review _NCName <$> _allID
          , (,) [qn|maxOccurs|] . review _One <$> _allMaxOccurs
          , (,) [qn|minOccurs|] <$>
            ((^? failing (_Left . re _Zero) (_Right . re _One)) =<< 
            _allMinOccurs)
          ]
        , teContents =
          [ TEContent (Fold $ allContent . folded) (Fold $ re _Element) ]
        }

      ea :: XML.Element -> Maybe (Namespaced All)
      ea XML.Element{..} = do
        let
          ns = elementName ^. XML._nameNamespace
        el <- case XML.nameLocalName elementName of
          "all" ->
            let
              reservedKeys = ["id", "maxOccurs", "minOccurs"]
              _allID =
                elementAttributes ^? at "id" . _Just . _NCName
              _allMaxOccurs =
                elementAttributes ^? at "maxOccurs" . _Just . _One
              _allMinOccurs =
                elementAttributes ^?
                at "minOccurs" .
                _Just .
                failing (_Zero . re _Left) (_One . re _Right)
              _allAttrs =
                createAttrs reservedKeys elementAttributes
              _allContent =
                elementNodes ^.. folded . XML._Element . _Element . _2
            in
            Just All{..}
          _ -> Nothing
        withNamespace ns el

class AsAny s where
  _Any :: Prism' s (Namespaced Any)

instance AsAny XML.Element where
  _Any = prism' ae ea
    where
      ae :: Namespaced Any -> XML.Element
      ae = toElement
        ToElement
        { teName = const "any"
        , teAttrs = \Any{..} ->
          [ (,) [qn|id|] . review _NCName <$> _anyID
          , (,) [qn|maxOccurs|] . review _Occurances <$> _anyMaxOccurs
          , (,) [qn|minOccurs|] . review _NonNegative <$> _anyMinOccurs
          , (,) [qn|namespace|] . review _Namespace <$> _anyNamespace
          , (,) [qn|processContents|] .
            review _ProcessContents <$> _anyProcessContents
          ]
        , teContents = []
        }

      ea :: XML.Element -> Maybe (Namespaced Any)
      ea XML.Element{..} = do
        let
          ns = elementName ^. XML._nameNamespace
        el <- case XML.nameLocalName elementName of
          "any" ->
            let
              reservedKeys = ["id", "maxOccurs", "minOccurs", "namespace", "processContents"]
              _anyID =
                elementAttributes ^? at "id" . _Just . _NCName
              _anyMaxOccurs =
                elementAttributes ^? at "maxOccurs" . _Just . _Occurances
              _anyMinOccurs =
                elementAttributes ^? at "minOccurs" . _Just . _NonNegative
              _anyNamespace =
                elementAttributes ^? at "namespace" . _Just . _Namespace
              _anyProcessContents =
                elementAttributes ^?
                at "processContents" .
                _Just .
                _ProcessContents
              _anyAttrs =
                createAttrs reservedKeys elementAttributes
            in
            Just Any{..}
          _ -> Nothing
        withNamespace ns el

class AsChoiceContent s where
  _ChoiceContent :: Prism' s (Namespaced ChoiceContent)

instance AsChoiceContent XML.Element where
  _ChoiceContent =
    prism'
      (\case
         (ns, CCElement e) -> review _Element (ns, e)
         (ns, CCGroup g) -> review _Group (ns, g)
         (ns, CCSequence s) -> review _Sequence (ns, s)
         (ns, CCAny a) -> review _Any (ns, a))
      (\e ->
         (second CCElement <$> preview _Element e) <|>
         (second CCGroup <$> preview _Group e) <|>
         (second CCSequence <$> preview _Sequence e) <|>
         (second CCAny <$> preview _Any e))
  
class AsChoice s where
  _Choice :: Prism' s (Namespaced Choice)
  
instance AsChoice XML.Element where
  _Choice = prism' ce ec
    where
      ce :: Namespaced Choice -> XML.Element
      ce = toElement
        ToElement
        { teName = const "choice"
        , teAttrs = \Choice{..} ->
          [ (,) [qn|id|] . review _NCName <$> _choiceID
          , (,) [qn|maxOccurs|] . review _Occurances <$> _choiceMaxOccurs
          , (,) [qn|minOccurs|] . review _NonNegative <$> _choiceMinOccurs
          ]
        , teContents =
          [ TEContent (Fold $ choiceContent . folded) (Fold $ re _ChoiceContent) ]
        }

      ec :: XML.Element -> Maybe (Namespaced Choice)
      ec XML.Element{..} = do
        let
          ns = elementName ^. XML._nameNamespace
        el <- case XML.nameLocalName elementName of
          "choice" ->
            let
              reservedKeys = ["id", "maxOccurs", "minOccurs"]
              _choiceID =
                elementAttributes ^? at "id" . _Just . _NCName
              _choiceMaxOccurs =
                elementAttributes ^? at "maxOccurs" . _Just . _Occurances
              _choiceMinOccurs =
                elementAttributes ^? at "minOccurs" . _Just . _NonNegative
              _choiceAttrs =
                createAttrs reservedKeys elementAttributes
              _choiceContent =
                elementNodes ^.. folded . XML._Element . _ChoiceContent . _2
            in
            Just Choice{..}
          _ -> Nothing
        withNamespace ns el

class AsSequenceContent s where
  _SequenceContent :: Prism' s (Namespaced SequenceContent)

instance AsSequenceContent XML.Element where
  _SequenceContent =
    prism'
      (\case
          (ns, SCElement e) -> review _Element (ns, e)
          (ns, SCGroup g) -> review _Group (ns, g)
          (ns, SCSequence s) -> review _Sequence (ns, s)
          (ns, SCAny a) -> review _Any (ns, a))

      (\e ->
         (second SCElement <$> preview _Element e) <|>
         (second SCGroup <$> preview _Group e) <|>
         (second SCSequence <$> preview _Sequence e) <|>
         (second SCAny <$> preview _Any e))

class AsSequence s where
  _Sequence :: Prism' s (Namespaced Sequence)
  
instance AsSequence XML.Element where
  _Sequence = prism' se es
    where
      se :: Namespaced Sequence -> XML.Element
      se = toElement
        ToElement
        { teName = const "sequence"
        , teAttrs = \Sequence{..} ->
          [ (,) [qn|id|] . review _NCName <$> _sequenceID
          , (,) [qn|maxOccurs|] . review _Occurances <$> _sequenceMaxOccurs
          , (,) [qn|minOccurs|] . review _NonNegative <$> _sequenceMinOccurs
          ]
        , teContents =
          [ TEContent (Fold $ sequenceContent . folded) (Fold $ re _SequenceContent) ]
        }

      es :: XML.Element -> Maybe (Namespaced Sequence)
      es XML.Element{..} = do
        let
          ns = elementName ^. XML._nameNamespace
        el <- case XML.nameLocalName elementName of
          "sequence" ->
            let
              reservedKeys = ["id", "maxOccurs", "minOccurs"]
              _sequenceID =
                elementAttributes ^? at "id" . _Just . _NCName
              _sequenceMaxOccurs =
                elementAttributes ^? at "maxOccurs" . _Just . _Occurances
              _sequenceMinOccurs =
                elementAttributes ^? at "minOccurs" . _Just . _NonNegative
              _sequenceAttrs =
                createAttrs reservedKeys elementAttributes
              _sequenceContent =
                elementNodes ^.. folded . XML._Element . _SequenceContent . _2
            in
            Just Sequence{..}
          _ -> Nothing
        withNamespace ns el

instance AsSequence (Namespaced CTGroupDefinition) where
  _Sequence =
    prism'
      (second CTGDSequence)
      (\case
          (ns, CTGDSequence a) -> Just (ns, a)
          _ -> Nothing)
      
instance AsSequence (Namespaced CTContent) where
  _Sequence = prism'
    (second $ \s -> emptyCTGroupContent { _ctgcGroupDefinition = Just (CTGDSequence s) })
    (\case
        (ns, CTGroupContent{..}) ->
          (_ctgcGroupDefinition ^? _Just . to ((,) ns) . _Sequence . _2) >>= pure . (,) ns
        _ -> Nothing)
    
instance AsSequence CTContent where
  _Sequence = prism'
    (\(_, s) -> emptyCTGroupContent { _ctgcGroupDefinition = Just (CTGDSequence s) })
    (\case
        CTGroupContent{..} ->
          (_ctgcGroupDefinition ^?
           _Just .
           to ((,) (Nothing :: Maybe NCName)) .
           _Sequence . _2) >>= pure . (,) Nothing
        _ -> Nothing)
  
class AsAttributeGroup s a where
  _AttributeGroup :: Prism' s (Namespaced a)

instance AsAttributeGroup (Namespaced AttributeGroup) AttributeGroup where
  _AttributeGroup = prism' id Just
  
instance AsAttributeGroup XML.Element AttributeGroup where
  _AttributeGroup = prism' agToElement elementToAg
    where
      agToElement :: Namespaced AttributeGroup -> XML.Element
      agToElement = toElement
        ToElement
        { teName = const "attributeGroup"
        , teAttrs = \AttributeGroup{..} ->
          [ (,) [qn|id|] . review _NCName <$> _agID
          , (,) [qn|name|] . review _NCName <$> _agName
          , (,) [qn|ref|] . review _QName <$> _agRef
          ]
        , teContents =
          [ TEContent
            (Fold $ agAttributeSpec . folded)
            (Fold $ re prodDist . re (without _Attribute _AttributeGroup) . bothEither)
          , TEContent (Fold $ agAnyAttribute . _Just) (Fold $ re _AnyAttribute)
          ]
        }

      elementToAg :: XML.Element -> Maybe (Namespaced AttributeGroup)
      elementToAg XML.Element{..} = do
        let
          ns = elementName ^. XML._nameNamespace
        el <- case XML.nameLocalName elementName of
          "attributeGroup" ->
            let
              reservedKeys = ["id", "name", "ref"]
              _agID =
                elementAttributes ^? at "id" . _Just . _NCName
              _agName =
                elementAttributes ^? at "name" . _Just . _NCName
              _agRef =
                elementAttributes ^? at "ref" . _Just . _QName
              _agAttrs =
                createAttrs reservedKeys elementAttributes
              _agAttributeSpec =
                elementNodes ^..
                folded .
                XML._Element .
                failing (_Attribute . _2 . re _Left) (_AttributeGroup . _2 . re _Right)
              _agAnyAttribute =
                elementNodes ^?
                folded .
                XML._Element .
                _AnyAttribute . _2
            in
            Just AttributeGroup{..}
          _ -> Nothing
        withNamespace ns el
      
instance AsAttributeGroup XML.Element SimpleAttributeGroup where
  _AttributeGroup = _SimpleAttributeGroup
  
class AsAttribute s where
  _Attribute :: Prism' s (Namespaced Attribute)

_Use :: Prism' Text Use
_Use =
  prism'
    (\case
        Optional -> "optional"
        Prohibited -> "prohibited"
        Required -> "required")

    (\case
        "optional" -> Just Optional
        "prohibited" -> Just Prohibited
        "required" -> Just Required
        _ -> Nothing)

instance AsAttribute XML.Element where
  _Attribute = prism' attToElement elementToAtt
    where
      attToElement :: Namespaced Attribute -> XML.Element
      attToElement = toElement
        ToElement
        { teName = const "attribute"
        , teAttrs = \Attribute{..} ->
          [ (,) [qn|default|] <$> _attDefault
          , (,) [qn|fixed|] <$> _attFixed
          , (,) [qn|form|] . review _Form <$> _attForm
          , (,) [qn|name|] . review _NCName <$> _attName
          , (,) [qn|ref|] . review _QName <$> _attRef
          , (,) [qn|type|] . review _QName <$> _attType
          , (,) [qn|use|] . review _Use <$> _attUse
          ]
        , teContents =
          [ TEContent (Fold $ attSimpleType . _Just) (Fold $ re _SimpleType) ]
        }

      elementToAtt :: XML.Element -> Maybe (Namespaced Attribute)
      elementToAtt XML.Element{..} = do
        let
          ns = elementName ^. XML._nameNamespace
        el <- case XML.nameLocalName elementName of
          "attribute" ->
            let
              reservedKeys = ["id", "default", "fixed", "form", "name", "ref", "type", "use"]
              _attID =
                elementAttributes ^? at "id" . _Just . _NCName
              _attDefault =
                elementAttributes ^? at "default" . _Just
              _attFixed =
                elementAttributes ^? at "fixed" . _Just
              _attForm =
                elementAttributes ^? at "form" . _Just . _Form
              _attName =
                elementAttributes ^? at "name" . _Just . _NCName
              _attRef =
                elementAttributes ^? at "ref" . _Just . _QName
              _attType =
                elementAttributes ^? at "type" . _Just . _QName
              _attUse =
                elementAttributes ^? at "use" . _Just . _Use
              _attAttrs =
                createAttrs reservedKeys elementAttributes
              _attSimpleType =
                elementNodes ^? folded . XML._Element . _SimpleType . _2
            in
            Just Attribute{..}
          _ -> Nothing
        withNamespace ns el

class AsNotation s where
  _Notation :: Prism' s (Namespaced Notation)

instance AsNotation XML.Element where
  _Notation = prism' ne en
    where
      ne :: Namespaced Notation -> XML.Element
      ne = toElement
        ToElement
        { teName = const "notation"
        , teAttrs = \Notation{..} ->
          [ (,) [qn|id|] . review _NCName <$> _notID
          , (,) [qn|name|] . review _NCName <$> _notName
          , (,) [qn|public|] . review _Token <$> _notPublic
          , (,) [qn|system|] . review _URI <$> _notSystem
          ]
        , teContents = []
        }

      en :: XML.Element -> Maybe (Namespaced Notation)
      en XML.Element{..} = do
        let
          ns = elementName ^. XML._nameNamespace
        el <- case XML.nameLocalName elementName of
          "notation" ->
            let
              reservedKeys = ["id", "name", "public", "system"]
              _notID =
                elementAttributes ^? at "id" . _Just . _NCName
              _notName =
                elementAttributes ^? at "name" . _Just . _NCName
              _notPublic =
                elementAttributes ^? at "public" . _Just . _Token
              _notSystem =
                elementAttributes ^? at "system" . _Just . _URI
              _notAttrs =
                createAttrs reservedKeys elementAttributes
            in
            Just Notation{..}
          _ -> Nothing
        withNamespace ns el

class AsRestriction s a where
  _Restriction :: Prism' s (Namespaced a)

instance AsRestriction XML.Element SimpleRestriction where
  _Restriction = _SimpleRestriction
  
instance AsRestriction XML.Element ComplexRestriction where
  _Restriction = _ComplexRestriction

class AsExtension s a where
  _Extension :: Prism' s (Namespaced a)

instance AsExtension XML.Element SimpleExtension where
  _Extension = _SimpleExtension
  
instance AsExtension XML.Element ComplexExtension where
  _Extension = _ComplexExtension

class AsElement s where
  _Element :: Prism' s (Namespaced Element)
  _Element' :: NCName -> Review s (Namespaced Element)
  
instance AsElement (Namespaced SequenceContent) where
  _Element =
    prism'
      (second SCElement)
      (\case
          (ns, SCElement a) -> Just (ns, a)
          _ -> Nothing)
  _Element' name = unto (second $ \el -> SCElement el { _elName = Just name })
  
instance AsElement SequenceContent where
  _Element =
    prism'
      (SCElement . snd)
      (\case
          SCElement a -> Just (Nothing, a)
          _ -> Nothing)
  _Element' name = unto (\(_, el) -> SCElement el { _elName = Just name })
  
instance AsElement XML.Element where
  _Element = prism' a b
    where
      a :: Namespaced Element -> XML.Element
      a = toElement
        ToElement
        { teName = const "element"
        , teAttrs = \Element{..} ->
          [ (,) [qn|id|] . review _NCName <$> _elID
          , (,) [qn|abstract|] . review _Boolean <$> _elAbstract
          , (,) [qn|form|] . review _Form <$> _elForm
          , (,) [qn|maxOccurs|] . review _Occurances <$> _elMaxOccurs
          , (,) [qn|minOccurs|] . review _NonNegative <$> _elMinOccurs
          , (,) [qn|name|] . review _NCName <$> _elName
          , (,) [qn|nillable|] . review _Boolean <$> _elNillable
          , (,) [qn|type|] . review _QName <$> _elTypeName
          ]
        , teContents =
          [ TEContent
            (Fold $ elTypeElement . _Just)
            (Fold $ re prodDist . re (without _SimpleType _ComplexType) . bothEither)
          ]
        }

      b :: XML.Element -> Maybe (Namespaced Element)
      b XML.Element{..} = do
        let
          ns = elementName ^. XML._nameNamespace
        el <- case XML.nameLocalName elementName of
          "element" ->
            let
              reservedKeys =
                [ "id"
                , "abstract"
                , "form"
                , "maxOccurs"
                , "minOccurs"
                , "name"
                , "nillable"
                , "type"
                ]
              _elID =
                elementAttributes ^? at "id" . _Just . _NCName
              _elAbstract =
                elementAttributes ^? at "abstract" . _Just . _Boolean
              _elForm =
                elementAttributes ^? at "form" . _Just . _Form
              _elMaxOccurs =
                elementAttributes ^? at "maxOccurs" . _Just . _Occurances
              _elMinOccurs =
                elementAttributes ^? at "minOccurs" . _Just . _NonNegative
              _elName =
                elementAttributes ^? at "name" . _Just . _NCName
              _elNillable =
                elementAttributes ^? at "nillable" . _Just . _Boolean
              _elTypeName =
                elementAttributes ^? at "type" . _Just . _QName
              _elTypeElement =
                elementNodes ^?
                folded .
                XML._Element .
                failing (_SimpleType . _2 . re _Left) (_ComplexType . _2 . re _Right)
              _elAttrs =
                createAttrs reservedKeys elementAttributes
            in
            Just Element{..}
          _ -> Nothing
        withNamespace ns el

  _Element' name =
    prism'
      (review _Element . (& _2 . elName .~ Just name))
      ((\e -> guard (e ^. _2 . elName == Just name) $> e) <=< (^? _Element))

class AsInclude s where
  _Include :: Prism' s (Namespaced Include)

instance AsInclude XML.Element where
  _Include = prism' ie ei
    where
      ie :: Namespaced Include -> XML.Element
      ie = toElement
        ToElement
        { teName = const "include"
        , teAttrs = \Include{..} ->
          [ (,) [qn|id|] . review _NCName <$> _incID
          , Just ([qn|schemaLocation|], review _URI _incSchemaLocation)
          ]
        , teContents = []
        }

      ei :: XML.Element -> Maybe (Namespaced Include)
      ei XML.Element{..} = do
        let
          ns = elementName ^. XML._nameNamespace
        el <- case XML.nameLocalName elementName of
          "include" -> do
            let
              _incID =
                elementAttributes ^? at "id" . _Just . _NCName
              _incAttrs =
                createAttrs ["id", "schemaLocation"] elementAttributes
                
            _incSchemaLocation <-
                elementAttributes ^? at "schemaLocation" . _Just . _URI
            pure Include{..}
          _ -> Nothing
        withNamespace ns el
  
class AsImport s where
  _Import :: Prism' s (Namespaced Import)
  
instance AsImport XML.Element where
  _Import = prism' ie ei
    where
      ie :: Namespaced Import -> XML.Element
      ie = toElement
        ToElement
        { teName = const "import"
        , teAttrs = \Import{..} ->
          [ (,) [qn|id|] . review _NCName <$> _impID
          , (,) [qn|namespace|] . review _URI <$> _impNamespace
          , (,) [qn|schemaLocation|] . review _URI <$> _impSchemaLocation
          ]
        , teContents = []
        }

      ei :: XML.Element -> Maybe (Namespaced Import)
      ei XML.Element{..} = do
        let
          ns = elementName ^. XML._nameNamespace
        el <- case XML.nameLocalName elementName of
          "import" ->
            let
              reservedKeys = ["id", "schemaLocation", "namespace"]
              _impID =
                elementAttributes ^? at "id" . _Just . _NCName
              _impAttrs =
                createAttrs reservedKeys elementAttributes
              _impSchemaLocation =
                elementAttributes ^? at "schemaLocation" . _Just . _URI
              _impNamespace =
                elementAttributes ^? at "namespace" . _Just . _URI
            in
            Just Import{..}
          _ -> Nothing
        withNamespace ns el

_RedefineContent :: Prism' XML.Element (Namespaced RedefineContent)
_RedefineContent =
  prism'
  (\case
      (ns, RCSimpleType s) -> review _SimpleType (ns, s)
      (ns, RCComplexType c) -> review _ComplexType (ns, c)
      (ns, RCGroup g) -> review _Group (ns, g)
      (ns, RCAttributeGroup a) -> review _AttributeGroup (ns, a))
  (\e ->
     (second RCSimpleType <$> preview _SimpleType e) <|>
     (second RCComplexType <$> preview _ComplexType e) <|>
     (second RCGroup <$> preview _Group e) <|>
     (second RCAttributeGroup <$> preview _AttributeGroup e))
  
class AsRedefine s where
  _Redefine :: Prism' s (Namespaced Redefine)

instance AsRedefine XML.Element where
  _Redefine = prism' rte etr
    where
      rte :: Namespaced Redefine -> XML.Element
      rte = toElement
        ToElement
        { teName = const "redefine"
        , teAttrs = \Redefine{..} ->
          [ (,) [qn|id|] . review _NCName <$> _redID
          , Just ([qn|schemaLocation|], review _URI _redSchemaLocation)
          ]
        , teContents =
          [ TEContent (Fold $ redContent . folded) (Fold $ re _RedefineContent) ]
        }

      etr :: XML.Element -> Maybe (Namespaced Redefine)
      etr XML.Element{..} = do
        let
          ns = elementName ^. XML._nameNamespace
        el <- case XML.nameLocalName elementName of
          "redefine" -> do
            let
              _redID =
                elementAttributes ^? at "id" . _Just . _NCName
              _redAttrs =
                createAttrs ["id", "schemaLocation"] elementAttributes
              _redContent =
                elementNodes ^..
                folded .
                XML._Element .
                _RedefineContent . _2
                
            _redSchemaLocation <-
              elementAttributes ^? at "schemaLocation" . _Just . _URI

            pure Redefine{..}
          _ -> Nothing
        withNamespace ns el
