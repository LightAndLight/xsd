module Text.XML.XSD.Component.ConstrainingFacet where

import Prelude (Bool)

import Data.Text (Text)

import Text.XML.XSD.Types.NonNegative
import Text.XML.XSD.Types.Positive
import Text.XML.XSD.Types.Regex

data WhiteSpace
  = WSPreserve
  | WSReplace
  | WSCollapse

data ConstrainingFacet
  = CFLength
  { _cfLengthValue :: NonNegative
  , _cfLengthFixed :: Bool
  }

  | CFMinLength
  { _cfMinLengthValue :: NonNegative
  , _cfMinLengthFixed :: Bool
  }
  
  | CFMaxLength
  { _cfMaxLengthValue :: NonNegative
  , _cfMaxLengthFixed :: Bool
  }
  
  | CFPattern
  { _cfPatternValue :: Regex
  }
  
  | CFEnumeration
  { _cfEnumerationValue :: [Text]
  }
  
  | CFWhiteSpace
  { _cfWhiteSpaceValue :: WhiteSpace
  , _cfWhiteSpaceFixed :: WhiteSpace
  }
  
  | CFMaxInclusive
  { _cfMaxInclusiveValue :: Text
  , _cfMaxInclusiveFixed :: Bool
  }
  
  | CFMaxExclusive
  { _cfMaxExclusiveValue :: Text
  , _cfMaxExclusiveFixed :: Bool
  }
  
  | CFMinInclusive
  { _cfMinInclusiveValue :: Text
  , _cfMinInclusiveFixed :: Bool
  }
  
  | CFMinExclusive
  { _cfMinExclusiveValue :: Text
  , _cfMinExclusiveFixed :: Bool
  }

  | CFTotalDigits
  { _cfTotalDigitsValue :: Positive
  , _cfTotalDigitsFixed :: Bool
  }
  
  | CFFractionDigits
  { _cfFractionDigitsValue :: NonNegative
  , _cfFractionDigitsFixed :: Bool
  }
