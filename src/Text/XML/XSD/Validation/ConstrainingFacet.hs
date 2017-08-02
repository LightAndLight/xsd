{-|
Module: Text.XML.XSD.Validation.ConstrainingFacet
Description: @Constraining Facet@ schema components

The @Constraining Facet@ schema components

https://www.w3.org/TR/xmlschema-2/#rf-facets
-}

module Text.XML.XSD.Validation.ConstrainingFacet where

import Prelude (Bool(..), Eq, Show)

import Data.Text (Text)

import Text.XML.XSD.Types.NonNegativeInteger
import Text.XML.XSD.Types.PositiveInteger
import Text.XML.XSD.Types.Regex

-- | Permitted values of 'value' for the 'whiteSpace' facet
data WhiteSpace
  = WSPreserve
  | WSReplace
  | WSCollapse
  deriving (Eq, Show)

sameFacetKind :: ConstrainingFacet -> ConstrainingFacet -> Bool
sameFacetKind CFLength{} CFLength{} = True
sameFacetKind CFMinLength{} CFMinLength{} = True
sameFacetKind CFMaxLength{} CFMaxLength{} = True
sameFacetKind CFPattern{} CFPattern{} = True
sameFacetKind CFEnumeration{} CFEnumeration{} = True
sameFacetKind CFWhiteSpace{} CFWhiteSpace{} = True
sameFacetKind CFMaxInclusive{} CFMaxInclusive{} = True
sameFacetKind CFMaxExclusive{} CFMaxExclusive{} = True
sameFacetKind CFMinInclusive{} CFMinInclusive{} = True
sameFacetKind CFMinExclusive{} CFMinExclusive{} = True
sameFacetKind CFTotalDigits{} CFTotalDigits{} = True
sameFacetKind CFFractionDigits{} CFFractionDigits{} = True

data ConstrainingFacet
  -- | 'length' constraining facet
  -- |
  -- | https://www.w3.org/TR/xmlschema-2/#dc-length
  = CFLength
  { _cfLengthValue :: NonNegativeInteger
  , _cfLengthFixed :: Bool
  }

  -- | 'minLength' constraining facet
  -- |
  -- | https://www.w3.org/TR/xmlschema-2/#dc-minLength
  | CFMinLength
  { _cfMinLengthValue :: NonNegativeInteger
  , _cfMinLengthFixed :: Bool
  }
  
  -- | 'maxLength' constraining facet
  -- |
  -- | https://www.w3.org/TR/xmlschema-2/#dc-maxLength
  | CFMaxLength
  { _cfMaxLengthValue :: NonNegativeInteger
  , _cfMaxLengthFixed :: Bool
  }
  
  -- | 'pattern' constraining facet
  -- |
  -- | https://www.w3.org/TR/xmlschema-2/#dc-pattern
  | CFPattern
  { _cfPatternValue :: Regex
  }
  
  -- | 'enumeration' constraining facet
  -- |
  -- | https://www.w3.org/TR/xmlschema-2/#dc-enumeration
  | CFEnumeration
  { _cfEnumerationValue :: [Text]
  }
  
  -- | 'whiteSpace' constraining facet
  -- |
  -- | https://www.w3.org/TR/xmlschema-2/#dc-whiteSpace
  | CFWhiteSpace
  { _cfWhiteSpaceValue :: WhiteSpace
  , _cfWhiteSpaceFixed :: Bool
  }
  
  -- | 'maxInclusive' constraining facet
  -- |
  -- | https://www.w3.org/TR/xmlschema-2/#dc-maxInclusive
  | CFMaxInclusive
  { _cfMaxInclusiveValue :: Text
  , _cfMaxInclusiveFixed :: Bool
  }
  
  -- | 'maxExclusive' constraining facet
  -- |
  -- | https://www.w3.org/TR/xmlschema-2/#dc-maxExclusive
  | CFMaxExclusive
  { _cfMaxExclusiveValue :: Text
  , _cfMaxExclusiveFixed :: Bool
  }
  
  -- | 'minInclusive' constraining facet
  -- |
  -- | https://www.w3.org/TR/xmlschema-2/#dc-minInclusive
  | CFMinInclusive
  { _cfMinInclusiveValue :: Text
  , _cfMinInclusiveFixed :: Bool
  }
  
  -- | 'minExclusive' constraining facet
  -- |
  -- | https://www.w3.org/TR/xmlschema-2/#dc-minExclusive
  | CFMinExclusive
  { _cfMinExclusiveValue :: Text
  , _cfMinExclusiveFixed :: Bool
  }

  -- | 'totalDigits' constraining facet
  -- |
  -- | https://www.w3.org/TR/xmlschema-2/#dc-totalDigits
  | CFTotalDigits
  { _cfTotalDigitsValue :: PositiveInteger
  , _cfTotalDigitsFixed :: Bool
  }
  
  -- | 'fractionDigits' constraining facet
  -- |
  -- | https://www.w3.org/TR/xmlschema-2/#dc-fractionDigits
  | CFFractionDigits
  { _cfFractionDigitsValue :: NonNegativeInteger
  , _cfFractionDigitsFixed :: Bool
  }
  deriving (Eq, Show)
