module Data.Json.Extended.Type where

import Prelude

data EJsonType
  = Null
  | String
  | Boolean
  | Integer
  | Decimal
  | Array
  | Map

derive instance eqEJsonType ∷ Eq EJsonType
derive instance ordEJsonType ∷ Ord EJsonType

instance showEJsonType ∷ Show EJsonType where
  show Null = "Null"
  show String = "String"
  show Boolean = "Boolean"
  show Integer = "Integer"
  show Decimal = "Decimal"
  show Array = "Array"
  show Map = "Map"
