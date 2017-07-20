{-#
language

ExistentialQuantification, LambdaCase, OverloadedStrings, QuasiQuotes, RankNTypes, RecordWildCards, MultiParamTypeClasses
#-}

module Text.XML.XSD.Lens where

import Prelude

import Control.Applicative
import Control.Monad
import Control.Lens hiding (Choice)
import Data.Functor
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.XML as XML
import qualified Text.XML.Lens as XML

import Text.XML.Attrs
import Text.XML.Boolean
import Text.XML.NCName
import Text.XML.NonNegative
import Text.XML.QName
import Text.XML.Regex
import Text.XML.Token
import Text.XML.URI
import Text.XML.XSD.Block
import Text.XML.XSD.Final
import Text.XML.XSD.Form
import Text.XML.XSD.Namespace
import Text.XML.XSD.ProcessContents
import Text.XML.XSD.Types

data ToElement a
  = ToElement
  { teName :: a -> XML.Name
  , teAttrs :: a -> [Maybe (QName, Text)]
  , teContents :: [ReifiedFold a XML.Element]
  }
  
toElement
  :: HasAttrs a
  => ToElement a
  -> a
  -> XML.Element
toElement te a =
  XML.Element
  { elementName = teName te a
  , elementAttributes =
      M.mapKeys qNameToName $
      (a ^. attrs) `M.union` M.fromList (teAttrs te a ^.. folded . _Just)
  , elementNodes =
      a ^.. foldMap ((. re XML._Element) . runFold) (teContents te)
  }

_ConstraintFacet :: Prism' XML.Element ConstraintFacet
_ConstraintFacet = prism' constraintFacetToElement elementToConstraintFacet
  where
    constraintFacetToElement :: ConstraintFacet -> XML.Element
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
                [ Just ([qn|value|], _QName # _cfEnumerationValue)
                ]
              CFWhiteSpace{..} ->
                [ Just ([qn|value|], _WhiteSpace # _cfWhiteSpaceValue)
                , (,) [qn|fixed|] . (_Boolean #) <$> _cfWhiteSpaceFixed
                ]
        , teContents = []
        }

    elementToConstraintFacet :: XML.Element -> Maybe ConstraintFacet
    elementToConstraintFacet XML.Element{..} =
      let
        i = elementAttributes ^? at "id" . _Just . _NCName
        f = elementAttributes ^? at "fixed" . _Just . _Boolean
        att = emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
      in 
      case elementName of
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
          let v = elementAttributes ^? at "value" . _Just . _QName
          in CFEnumeration i <$> v <*> pure att

        "whiteSpace" -> 
          let v = elementAttributes ^? at "value" . _Just . _WhiteSpace
          in CFWhiteSpace i <$> v <*> pure f <*> pure att

        _ -> Nothing
  
_SimpleTypeContent :: Prism' XML.Element STContent
_SimpleTypeContent = prism' stContentToElement elementToStContent
  where
    stContentToElement :: STContent -> XML.Element
    stContentToElement stc =
      let te = case stc of
            STRestriction{} ->
              ToElement
              { teName = elementName
              , teAttrs = elementAttrs
              , teContents = 
                  [ Fold $ to _strsTypeElement . _Just . re _SimpleType
                  , Fold $ to _strsConstraints . folded . re _ConstraintFacet
                  ]
              }
            STList{} ->
              ToElement
              { teName = elementName
              , teAttrs = elementAttrs
              , teContents =
                  [Fold $ to _stlsTypeElement . _Just . re _SimpleType]
              }
            STUnion{} ->
              ToElement
              { teName = elementName
              , teAttrs = elementAttrs
              , teContents =
                [Fold $ to _stunTypeElements . _Just . folded . re _SimpleType]
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

    elementToStContent :: XML.Element -> Maybe STContent
    elementToStContent XML.Element{..} =
      let
        i = elementAttributes ^? at "id" . _Just . _NCName
        att = emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
      in case elementName of
        "restriction" ->
          let
            b = elementAttributes ^? at "base" . _Just . _QName
            st =
              elementNodes ^..
              folded .
              XML._Element .
              _SimpleType
            cfs = 
              elementNodes ^..
              folded .
              XML._Element .
              _ConstraintFacet
          in
          Just $ STRestriction i att b (st ^? _head) cfs

        "list" ->
          let
            it = elementAttributes ^? at "itemType" . _Just . _QName
            st = elementNodes ^.. folded . XML._Element . _SimpleType
          in
          Just $ STList i att it (st ^? _head)

        "union" ->
          let
            mts =
              toListOf (_Just . to T.words . folded . _QName) <$>
              (elementAttributes ^? at "memberTypes") 
            sts = 
              elementNodes ^..
              folded .
              XML._Element .
              _SimpleType
          in
          Just $ STUnion i att mts (if null sts then Nothing else Just sts)

        _ -> Nothing

class AsSimpleType s where
  _SimpleType :: Prism' s SimpleType

instance AsSimpleType XML.Element where
  _SimpleType = prism' simpleTypeToElement elementToSimpleType
    where
      simpleTypeToElement :: SimpleType -> XML.Element
      simpleTypeToElement =
        toElement
        ToElement
        { teName = const "simpleType"
        , teAttrs = \SimpleType{..} ->
            [ (,) [qn|name|] . review _NCName <$> _stName
            , (,) [qn|final|] . review _Final <$> _stFinal
            ]
        , teContents = [Fold $ stContent . re _SimpleTypeContent]
        }

      elementToSimpleType :: XML.Element -> Maybe SimpleType
      elementToSimpleType XML.Element{..} =
        let
          _stID = elementAttributes ^? at "id" . _Just . _NCName
          _stAttrs = emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
          _stFinal = elementAttributes ^? at "final" . _Just . _Final
          _stName = elementAttributes ^? at "name" . _Just . _NCName
          maybeStContent =
            (elementNodes ^.. folded . XML._Element . _SimpleTypeContent) ^? _head
        in do
          _stContent <- maybeStContent
          case elementName of
            "simpleType" -> Just SimpleType{..}
            _ -> Nothing

_SimpleAttributeGroup :: Prism' XML.Element SimpleAttributeGroup
_SimpleAttributeGroup = prism' sagToElement elementToSag
  where
    sagToElement :: SimpleAttributeGroup -> XML.Element
    sagToElement = toElement
      ToElement
      { teName = const "attributeGroup"
      , teAttrs = \SimpleAttributeGroup{..} ->
          [ (,) [qn|ref|] . review _QName <$> _sagRef ]
      , teContents = []
      }

    elementToSag :: XML.Element -> Maybe SimpleAttributeGroup
    elementToSag XML.Element{..} =
      case elementName of
        "attributeGroup" ->
          let
            _sagID = elementAttributes ^? at "id" . _Just . _NCName
            _sagRef = elementAttributes ^? at "ref" . _Just . _QName
            _sagAttrs = emptyAttrs & attrs .~
              M.mapKeys nameToQName elementAttributes
          in
          Just SimpleAttributeGroup{..}
        _ -> Nothing

_AnyAttribute :: Prism' XML.Element AnyAttribute
_AnyAttribute = prism' aaToElement elementToAa
  where
    aaToElement :: AnyAttribute -> XML.Element
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

    elementToAa :: XML.Element -> Maybe AnyAttribute
    elementToAa XML.Element{..} =
      case elementName of
        "anyAttribute" ->
          let
            _aaID =
              elementAttributes ^? at "id" . _Just . _NCName
            _aaNamespace =
              elementAttributes ^? at "namespace" . _Just . _Namespace
            _aaProcessContents = 
              elementAttributes ^? at "processContents" . _Just . _ProcessContents
            _aaAttrs =
              emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
          in
          Just AnyAttribute{..}
        _ -> Nothing

_SimpleRestriction :: Prism' XML.Element SimpleRestriction
_SimpleRestriction = prism' srToElement elementToSr
  where
    srToElement :: SimpleRestriction -> XML.Element
    srToElement = toElement
      ToElement
      { teName = const "restriction"
      , teAttrs = \SimpleRestriction{..} ->
        [ (,) [qn|base|] . review _QName <$> _srsBase ]
      , teContents =
        [ Fold $ srsType . _Just . re _SimpleType
        , Fold $ srsConstraints . folded . re _ConstraintFacet
        , Fold $
          srsAttributeSpec .
          folded .
          re (without _Attribute _SimpleAttributeGroup) .
          bothEither
        , Fold $ srsAnyAttribute . _Just . re _AnyAttribute
        ]
      }

    elementToSr :: XML.Element -> Maybe SimpleRestriction
    elementToSr XML.Element{..} =
      case elementName of
        "restriction" ->
          let
            _srsBase = elementAttributes ^? at "base" . _Just . _QName
            _srsID = elementAttributes ^? at "id" . _Just . _NCName
            _srsAttrs =
              emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
            _srsType = elementNodes ^? folded . XML._Element . _SimpleType
            _srsConstraints =
              elementNodes ^.. folded . XML._Element . _ConstraintFacet
            _srsAttributeSpec =
              elementNodes ^..
              folded .
              XML._Element .
              failing
                (_Attribute . re _Left)
                (_SimpleAttributeGroup . re _Right)
            _srsAnyAttribute =
              elementNodes ^? folded . XML._Element . _AnyAttribute
          in
          Just SimpleRestriction{..}
        _ -> Nothing

_SimpleExtension :: Prism' XML.Element SimpleExtension
_SimpleExtension = prism' seToElement elementToSe
  where
    seToElement :: SimpleExtension -> XML.Element
    seToElement = toElement
      ToElement
      { teName = const "extension"
      , teAttrs = \SimpleExtension{..} ->
          [ (,) [qn|base|] . review _QName <$> _sexBase ]
      , teContents =
        [ Fold $
          sexAttributeSpec .
          folded .
          re (without _Attribute _SimpleAttributeGroup) .
          bothEither
        , Fold $ sexAnyAttribute . _Just . re _AnyAttribute
        ]
      }

    elementToSe :: XML.Element -> Maybe SimpleExtension
    elementToSe XML.Element{..} =
      case elementName of
        "extension" ->
          let
            _sexBase = elementAttributes ^? at "base" . _Just . _QName
            _sexID = elementAttributes ^? at "id" . _Just . _NCName
            _sexAttrs =
              emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
            _sexAttributeSpec =
              elementNodes ^..
              folded .
              XML._Element .
              failing
                (_Attribute . re _Left)
                (_SimpleAttributeGroup . re _Right)
            _sexAnyAttribute =
              elementNodes ^? folded . XML._Element . _AnyAttribute
          in
          Just SimpleExtension{..}
        _ -> Nothing

_CTGroupDefinition :: Prism' XML.Element CTGroupDefinition
_CTGroupDefinition =
  prism'
    (\case
        CTGDGroup g -> review _Group g
        CTGDAll a -> review _All a
        CTGDChoice c -> review _Choice c
        CTGDSequence s -> review _Sequence s)

    $ \e ->
        (CTGDGroup <$> e ^? _Group) <|>
        (CTGDAll <$> e ^? _All) <|>
        (CTGDChoice <$> e ^? _Choice) <|>
        (CTGDSequence <$> e ^? _Sequence)

_ComplexRestriction :: Prism' XML.Element ComplexRestriction
_ComplexRestriction = prism' crToElement elementToCr
  where
    crToElement :: ComplexRestriction -> XML.Element
    crToElement = toElement
      ToElement
      { teName = const "restriction"
      , teAttrs = \ComplexRestriction{..} ->
        [ (,) [qn|base|] . review _QName <$> _crsBase ]
      , teContents =
        [ Fold $ crsGroupDefinition . _Just . re _CTGroupDefinition
        , Fold $
          crsAttributeSpec .
          folded .
          re (without _Attribute _AttributeGroup) .
          bothEither
        , Fold $ crsAnyAttribute . _Just . re _AnyAttribute
        ]
      }

    elementToCr :: XML.Element -> Maybe ComplexRestriction
    elementToCr XML.Element{..} =
      case elementName of
        "restriction" ->
          let
            _crsID =
              elementAttributes ^? at "id" . _Just . _NCName
            _crsBase =
              elementAttributes ^? at "base" . _Just . _QName
            _crsAttrs =
              emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
            _crsGroupDefinition =
              elementNodes ^? folded . XML._Element . _CTGroupDefinition
            _crsAttributeSpec =
              elementNodes ^..
              folded .
              XML._Element .
              failing (_Attribute . re _Left) (_AttributeGroup . re _Right)
            _crsAnyAttribute =
              elementNodes ^? folded . XML._Element . _AnyAttribute
          in
          Just ComplexRestriction{..}
        _ -> Nothing

_ComplexExtension :: Prism' XML.Element ComplexExtension
_ComplexExtension = prism' ce ec
  where
    ce :: ComplexExtension -> XML.Element
    ce = toElement
      ToElement
      { teName = const "extension"
      , teAttrs = \ComplexExtension{..} ->
          [ (,) [qn|base|] . review _QName <$> _cexBase ]
      , teContents =
          [ Fold $ cexGroupDefinition . _Just . re _CTGroupDefinition
          , Fold $
            cexAttributeSpec .
            folded .
            re (without _Attribute _AttributeGroup) .
            bothEither
          , Fold $ cexAnyAttribute . _Just . re _AnyAttribute
          ]
      }

    ec :: XML.Element -> Maybe ComplexExtension
    ec XML.Element{..} =
      case elementName of
        "extension" ->
          let
            _cexID =
              elementAttributes ^? at "id" . _Just . _NCName
            _cexBase =
              elementAttributes ^? at "base" . _Just . _QName
            _cexAttrs =
              emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
            _cexGroupDefinition =
              elementNodes ^? folded . XML._Element . _CTGroupDefinition
            _cexAttributeSpec =
              elementNodes ^..
              folded .
              XML._Element .
              failing (_Attribute . re _Left) (_AttributeGroup . re _Right)
            _cexAnyAttribute =
              elementNodes ^? folded . XML._Element . _AnyAttribute
          in
          Just ComplexExtension{..}
        _ -> Nothing

bothEither :: Iso' (Either a a) a
bothEither = iso (either id id) Left

_CTContent :: Prism' [XML.Element] CTContent
_CTContent = prism' ctContentToElements elementsToCtContent
  where
    ctContentToElements :: CTContent -> [XML.Element]
    ctContentToElements c =
      case c of
        CTSimpleContent{..} ->
          [ XML.Element
            { elementName = "simpleContent"
            , elementAttributes = toNameTextMap _ctscAttrs
            , elementNodes =
                [ case _ctscContent of
                    Left a -> review (XML._Element . _SimpleRestriction) a
                    Right b -> review (XML._Element . _SimpleExtension) b
                ]
            }
          ]
        CTComplexContent{..} ->
          [ XML.Element
            { elementName = "complexContent"
            , elementAttributes = toNameTextMap _ctccAttrs
            , elementNodes =
                [ case _ctccContent of
                    Left a -> review (XML._Element . _ComplexRestriction) a
                    Right b -> review (XML._Element . _ComplexExtension) b
                ]
            }
          ]
        CTGroupContent{..} ->
          (_ctgcGroupDefinition ^.. _Just . re _CTGroupDefinition) <>
          (_ctgcAttributeSpec ^..
            folded . re (without _Attribute _AttributeGroup) . bothEither) <>
          (_ctgcAnyAttribute ^.. _Just . re _AnyAttribute)

    elementsToCtContent :: [XML.Element] -> Maybe CTContent
    elementsToCtContent [] = Nothing
    elementsToCtContent es =
      let
        simpleContent = do
          XML.Element{..} <- 
            es ^? folded . filtered ((==) "simpleContent" . XML.elementName)
          let
            _ctscID = elementAttributes ^? at "id" . _Just . _NCName
            _ctscAttrs =
              emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
          _ctscContent <-
            elementNodes ^?
              folded .
              XML._Element .
              failing
                (_SimpleRestriction . re _Left)
                (_SimpleExtension . re _Right)
          pure CTSimpleContent{..}
             
        complexContent = do
          XML.Element{..} <- 
            es ^? folded . filtered ((==) "complexContent" . XML.elementName)
          let
            _ctccID = elementAttributes ^? at "id" . _Just . _NCName
            _ctccMixed = elementAttributes ^? at "mixed" . _Just . _Boolean
            _ctccAttrs =
              emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
          _ctccContent <-
            elementNodes ^?
              folded .
              XML._Element .
              failing
                (_ComplexRestriction . re _Left)
                (_ComplexExtension . re _Right)
          pure CTComplexContent{..}
          
        groupContent =
          let
            _ctgcGroupDefinition =
              es ^? folded . _CTGroupDefinition
            _ctgcAttributeSpec =
              es ^..
              folded .
              failing (_Attribute . re _Left) (_AttributeGroup . re _Right)
            _ctgcAnyAttribute =
              es ^? folded . _AnyAttribute
          in CTGroupContent{..}
      in simpleContent <|> complexContent <|> pure groupContent

class AsComplexType s where
  _ComplexType :: Prism' s ComplexType

instance AsComplexType XML.Element where
  _ComplexType = prism' complexTypeToElement elementToComplexType
    where
      complexTypeToElement :: ComplexType -> XML.Element
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
        , teContents = [Fold $ ctContent . re _CTContent . folded]
        }

      elementToComplexType :: XML.Element -> Maybe ComplexType
      elementToComplexType XML.Element{..} =
        case elementName of
          "complexType" -> do
            let
              _ctID =
                elementAttributes ^? at "id" . _Just . _NCName
              _ctAttrs =
                emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
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
              elementNodes ^? to (^.. folded . XML._Element) . _CTContent
            Just ComplexType{..}
          _ -> Nothing

_GroupContent :: Prism' XML.Element GroupContent
_GroupContent = prism' ge eg
  where
    ge :: GroupContent -> XML.Element
    ge = \case
      GCAll a -> review _All a
      GCChoice c -> review _Choice c
      GCSequence s -> review _Sequence s

    eg :: XML.Element -> Maybe GroupContent
    eg e =
      (GCAll <$> e ^? _All) <|>
      (GCChoice <$> e ^? _Choice) <|>
      (GCSequence <$> e ^? _Sequence)

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
  _Group :: Prism' s Group

instance AsGroup XML.Element where
  _Group = prism' ge eg
    where
      ge :: Group -> XML.Element
      ge = toElement
        ToElement
        { teName = const "group"
        , teAttrs = \Group{..} ->
          [ (,) [qn|maxOccurs|] . review _Occurances <$> _grMaxOccurs
          , (,) [qn|minOccurs|] . review _NonNegative <$> _grMinOccurs
          , (,) [qn|name|] . review _NCName <$> _grName
          , (,) [qn|ref|] . review _QName <$> _grRef
          ]
        , teContents = [ Fold $ grContent . _Just . re _GroupContent ] 
        }

      eg :: XML.Element -> Maybe Group
      eg XML.Element{..} =
        case elementName of
          "group" ->
            let
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
                emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
              _grContent =
                elementNodes ^? folded . XML._Element . _GroupContent
            in
            Just Group{..}
          _ -> Nothing

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
  _All :: Prism' s All

instance AsAll XML.Element where
  _All = prism' ae ea
    where
      ae :: All -> XML.Element
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
          [ Fold $ allContent . folded . re _Element ]
        }

      ea :: XML.Element -> Maybe All
      ea XML.Element{..} =
        case elementName of
          "all" ->
            let
              _allID =
                elementAttributes ^? at "id" . _Just . _NCName
              _allMaxOccurs =
                elementAttributes ^? at "maxOccurs" . _Just . _One
              _allMinOccurs =
                elementAttributes ^?
                at "maxOccurs" .
                _Just .
                failing (_Zero . re _Left) (_One . re _Right)
              _allAttrs =
                emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
              _allContent =
                elementNodes ^.. folded . XML._Element . _Element
            in
            Just All{..}
          _ -> Nothing

class AsAny s where
  _Any :: Prism' s Any

instance AsAny XML.Element where
  _Any = prism' ae ea
    where
      ae :: Any -> XML.Element
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

      ea :: XML.Element -> Maybe Any
      ea XML.Element{..} =
        case elementName of
          "any" ->
            let
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
                emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
            in
            Just Any{..}
          _ -> Nothing

class AsChoiceContent s where
  _ChoiceContent :: Prism' s ChoiceContent

instance AsChoiceContent XML.Element where
  _ChoiceContent =
    prism'
      (\case
         CCElement e -> review _Element e
         CCGroup g -> review _Group g
         CCSequence s -> review _Sequence s
         CCAny a -> review _Any a)
      (\e ->
         (CCElement <$> preview _Element e) <|>
         (CCGroup <$> preview _Group e) <|>
         (CCSequence <$> preview _Sequence e) <|>
         (CCAny <$> preview _Any e))
  
class AsChoice s where
  _Choice :: Prism' s Choice
  
instance AsChoice XML.Element where
  _Choice = prism' ce ec
    where
      ce :: Choice -> XML.Element
      ce = toElement
        ToElement
        { teName = const "choice"
        , teAttrs = \Choice{..} ->
          [ (,) [qn|id|] . review _NCName <$> _choiceID
          , (,) [qn|maxOccurs|] . review _Occurances <$> _choiceMaxOccurs
          , (,) [qn|minOccurs|] . review _NonNegative <$> _choiceMinOccurs
          ]
        , teContents =
          [ Fold $ choiceContent . folded . re _ChoiceContent ]
        }

      ec :: XML.Element -> Maybe Choice
      ec XML.Element{..} =
        case elementName of
          "choice" ->
            let
              _choiceID =
                elementAttributes ^? at "id" . _Just . _NCName
              _choiceMaxOccurs =
                elementAttributes ^? at "maxOccurs" . _Just . _Occurances
              _choiceMinOccurs =
                elementAttributes ^? at "maxOccurs" . _Just . _NonNegative
              _choiceAttrs =
                emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
              _choiceContent =
                elementNodes ^.. folded . XML._Element . _ChoiceContent
            in
            Just Choice{..}
          _ -> Nothing

class AsSequenceContent s where
  _SequenceContent :: Prism' s SequenceContent

instance AsSequenceContent XML.Element where
  _SequenceContent =
    prism'
      (\case
          SCElement e -> review _Element e
          SCGroup g -> review _Group g
          SCSequence s -> review _Sequence s
          SCAny a -> review _Any a)

      (\e ->
         (SCElement <$> preview _Element e) <|>
         (SCGroup <$> preview _Group e) <|>
         (SCSequence <$> preview _Sequence e) <|>
         (SCAny <$> preview _Any e))

class AsSequence s where
  _Sequence :: Prism' s Sequence
  
instance AsSequence XML.Element where
  _Sequence = prism' se es
    where
      se :: Sequence -> XML.Element
      se = toElement
        ToElement
        { teName = const "sequence"
        , teAttrs = \Sequence{..} ->
          [ (,) [qn|id|] . review _NCName <$> _sequenceID
          , (,) [qn|maxOccurs|] . review _Occurances <$> _sequenceMaxOccurs
          , (,) [qn|minOccurs|] . review _NonNegative <$> _sequenceMinOccurs
          ]
        , teContents =
          [ Fold $ sequenceContent . folded . re _SequenceContent ]
        }

      es :: XML.Element -> Maybe Sequence
      es XML.Element{..} =
        case elementName of
          "sequence" ->
            let
              _sequenceID =
                elementAttributes ^? at "id" . _Just . _NCName
              _sequenceMaxOccurs =
                elementAttributes ^? at "maxOccurs" . _Just . _Occurances
              _sequenceMinOccurs =
                elementAttributes ^? at "maxOccurs" . _Just . _NonNegative
              _sequenceAttrs =
                emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
              _sequenceContent =
                elementNodes ^.. folded . XML._Element . _SequenceContent
            in
            Just Sequence{..}
          _ -> Nothing

instance AsSequence CTGroupDefinition where
  _Sequence = prism'
    CTGDSequence $
    \case
      CTGDSequence a -> Just a
      _ -> Nothing
      
instance AsSequence CTContent where
  _Sequence = prism'
    (\v -> emptyCTGroupContent { _ctgcGroupDefinition = Just (_Sequence # v) })
    (maybe Nothing (^? _Sequence) . _ctgcGroupDefinition)
  
class AsAttributeGroup s a where
  _AttributeGroup :: Prism' s a

instance AsAttributeGroup AttributeGroup AttributeGroup where
  _AttributeGroup = prism' id Just
  
instance AsAttributeGroup XML.Element AttributeGroup where
  _AttributeGroup = prism' agToElement elementToAg
    where
      agToElement :: AttributeGroup -> XML.Element
      agToElement = toElement
        ToElement
        { teName = const "attributeGroup"
        , teAttrs = \AttributeGroup{..} ->
          [ (,) [qn|id|] . review _NCName <$> _agID
          , (,) [qn|name|] . review _NCName <$> _agName
          , (,) [qn|ref|] . review _QName <$> _agRef
          ]
        , teContents =
          [ Fold $
            agAttributeSpec .
            folded .
            re (without _Attribute _AttributeGroup) .
            bothEither
          , Fold $ agAnyAttribute . _Just . re _AnyAttribute
          ]
        }

      elementToAg :: XML.Element -> Maybe AttributeGroup
      elementToAg XML.Element{..} =
        case elementName of
          "attributeGroup" ->
            let
              _agID =
                elementAttributes ^? at "id" . _Just . _NCName
              _agName =
                elementAttributes ^? at "name" . _Just . _NCName
              _agRef =
                elementAttributes ^? at "ref" . _Just . _QName
              _agAttrs =
                emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
              _agAttributeSpec =
                elementNodes ^..
                folded .
                XML._Element .
                failing (_Attribute . re _Left) (_AttributeGroup . re _Right)
              _agAnyAttribute =
                elementNodes ^?
                folded .
                XML._Element .
                _AnyAttribute
            in
            Just AttributeGroup{..}
          _ -> Nothing
      
instance AsAttributeGroup XML.Element SimpleAttributeGroup where
  _AttributeGroup = _SimpleAttributeGroup
  
class AsAttribute s where
  _Attribute :: Prism' s Attribute

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
      attToElement :: Attribute -> XML.Element
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
          [ Fold $ attSimpleType . _Just . re _SimpleType ]
        }

      elementToAtt :: XML.Element -> Maybe Attribute
      elementToAtt XML.Element{..} =
        case elementName of
          "attribute" ->
            let
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
                emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
              _attSimpleType =
                elementNodes ^? folded . XML._Element . _SimpleType
            in
            Just Attribute{..}
          _ -> Nothing

class AsNotation s where
  _Notation :: Prism' s Notation

instance AsNotation XML.Element where
  _Notation = prism' ne en
    where
      ne :: Notation -> XML.Element
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

      en :: XML.Element -> Maybe Notation
      en XML.Element{..} =
        case elementName of
          "notation" ->
            let
              _notID =
                elementAttributes ^? at "id" . _Just . _NCName
              _notName =
                elementAttributes ^? at "name" . _Just . _NCName
              _notPublic =
                elementAttributes ^? at "public" . _Just . _Token
              _notSystem =
                elementAttributes ^? at "system" . _Just . _URI
              _notAttrs =
                emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
            in
            Just Notation{..}
          _ -> Nothing

class AsRestriction s a where
  _Restriction :: Prism' s a

instance AsRestriction XML.Element SimpleRestriction where
  _Restriction = _SimpleRestriction
  
instance AsRestriction XML.Element ComplexRestriction where
  _Restriction = _ComplexRestriction

class AsExtension s a where
  _Extension :: Prism' s a

instance AsExtension XML.Element SimpleExtension where
  _Extension = _SimpleExtension
  
instance AsExtension XML.Element ComplexExtension where
  _Extension = _ComplexExtension

class AsElement s where
  _Element :: Prism' s Element
  _Element' :: NCName -> Review s Element
  
instance AsElement SequenceContent where
  _Element = prism' SCElement $
    \case
      SCElement a -> Just a
      _ -> Nothing
  _Element' name = unto (\el -> SCElement el { _elName = Just name })
  
instance AsElement XML.Element where
  _Element = prism' a b
    where
      a :: Element -> XML.Element
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
          [ Fold $
            elTypeElement .
            _Just .
            re (without _SimpleType _ComplexType) .
            bothEither
          ]
        }

      b :: XML.Element -> Maybe Element
      b XML.Element{..} =
        case elementName of
          "element" ->
            let
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
                failing (_SimpleType . re _Left) (_ComplexType . re _Right)
              _elAttrs =
                emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
            in
            Just Element{..}
          _ -> Nothing

  _Element' name =
    prism'
      (review _Element . (& elName .~ Just name))
      ((\e -> guard (e ^. elName == Just name) $> e) <=< (^? _Element))

class AsInclude s where
  _Include :: Prism' s Include

instance AsInclude XML.Element where
  _Include = prism' ie ei
    where
      ie :: Include -> XML.Element
      ie = toElement
        ToElement
        { teName = const "include"
        , teAttrs = \Include{..} ->
          [ (,) [qn|id|] . review _NCName <$> _incID
          , Just ([qn|schemaLocation|], review _URI _incSchemaLocation)
          ]
        , teContents = []
        }

      ei :: XML.Element -> Maybe Include
      ei XML.Element{..} =
        case elementName of
          "include" -> do
            let
              _incID =
                elementAttributes ^? at "id" . _Just . _NCName
              _incAttrs =
                emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
                
            _incSchemaLocation <-
                elementAttributes ^? at "schemaLocation" . _Just . _URI
            pure Include{..}
          _ -> Nothing
  
class AsImport s where
  _Import :: Prism' s Import
  
instance AsImport XML.Element where
  _Import = prism' ie ei
    where
      ie :: Import -> XML.Element
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

      ei :: XML.Element -> Maybe Import
      ei XML.Element{..} =
        case elementName of
          "import" ->
            let
              _impID =
                elementAttributes ^? at "id" . _Just . _NCName
              _impAttrs =
                emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
              _impSchemaLocation =
                elementAttributes ^? at "schemaLocation" . _Just . _URI
              _impNamespace =
                elementAttributes ^? at "namespace" . _Just . _URI
            in
            Just Import{..}
          _ -> Nothing

_RedefineContent :: Prism' XML.Element RedefineContent
_RedefineContent =
  prism'
  (\case
      RCSimpleType s -> review _SimpleType s
      RCComplexType c -> review _ComplexType c
      RCGroup g -> review _Group g
      RCAttributeGroup a -> review _AttributeGroup a)
  (\e ->
     (RCSimpleType <$> preview _SimpleType e) <|>
     (RCComplexType <$> preview _ComplexType e) <|>
     (RCGroup <$> preview _Group e) <|>
     (RCAttributeGroup <$> preview _AttributeGroup e))
  
class AsRedefine s where
  _Redefine :: Prism' s Redefine

instance AsRedefine XML.Element where
  _Redefine = prism' rte etr
    where
      rte :: Redefine -> XML.Element
      rte = toElement
        ToElement
        { teName = const "redefine"
        , teAttrs = \Redefine{..} ->
          [ (,) [qn|id|] . review _NCName <$> _redID
          , Just ([qn|schemaLocation|], review _URI _redSchemaLocation)
          ]
        , teContents =
          [ Fold $ redContent . folded . re _RedefineContent ]
        }

      etr :: XML.Element -> Maybe Redefine
      etr XML.Element{..} =
        case elementName of
          "redefine" -> do
            let
              _redID =
                elementAttributes ^? at "id" . _Just . _NCName
              _redAttrs =
                emptyAttrs & attrs .~ M.mapKeys nameToQName elementAttributes
              _redContent =
                elementNodes ^..
                folded .
                XML._Element .
                _RedefineContent
                
            _redSchemaLocation <-
              elementAttributes ^? at "schemaLocation" . _Just . _URI

            pure Redefine{..}
          _ -> Nothing
