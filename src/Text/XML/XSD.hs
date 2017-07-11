{-# language GeneralizedNewtypeDeriving #-}
module Text.XML.XSD () where

import Prelude

-- | 'restriction' element https://www.w3.org/TR/xmlschema-1/#element-restriction 
data Restriction
  = Restriction
  { _rsBase :: Maybe QName
  , _rsID :: Maybe ID
  , _rsAttrs
  , _rsSimpleType :: Maybe SimpleType
  , _rsAnnotation :: Maybe Annotation
  , _rsConstraints :: [ConstraintFacet]
  }

-- | 'list' element https://www.w3.org/TR/xmlschema-1/#element-list 
data List
  = List
  { _lsID :: Maybe ID
  , _lsItemType :: Maybe QName
  , _lsSimpleType :: Maybe SimpleType
  , _lsAttrs :: Map Name Text
  , _lsAnnotation :: Maybe Annotation
  }

-- | 'union' element https://www.w3.org/TR/xmlschema-1/#element-union 
data Union
  = Union
  { _unID :: Maybe ID
  , _unMemberTypes :: [QName]
  }
