{-# LANGUAGE OverloadedStrings #-}

module Kuroko.Core.Schema
  ( objectSchema
  , stringProp
  , intProp
  , boolProp
  , arrayProp
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)

-- | Construct a JSON Schema object specification for tool parameters.
-- Takes a list of (fieldName, propertyType, description, isRequired).
objectSchema :: [(Text, Value, Text, Bool)] -> Value
objectSchema fields =
  object
    [ "type" .= ("object" :: Text)
    , "properties" .= object [ K.fromText name .= propObj ty desc | (name, ty, desc, _) <- fields ]
    , "required" .= [ name | (name, _, _, req) <- fields, req ]
    ]
  where
    propObj ty desc = case ty of
      Object km -> Object (KM.insert "description" (String desc) km)
      _         -> object [ "type" .= ty, "description" .= desc ]

-- | String property type schema.
stringProp :: Value
stringProp = object [ "type" .= ("string" :: Text) ]

-- | Integer property type schema.
intProp :: Value
intProp = object [ "type" .= ("integer" :: Text) ]

-- | Boolean property type schema.
boolProp :: Value
boolProp = object [ "type" .= ("boolean" :: Text) ]

-- | Array property type schema given element type schema.
arrayProp :: Value -> Value
arrayProp itemType =
  object
    [ "type" .= ("array" :: Text)
    , "items" .= itemType
    ]
