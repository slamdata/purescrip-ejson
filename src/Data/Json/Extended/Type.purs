module Data.Json.Extended.Type where

import Prelude

data EJsonType
  = Null
  | String
  | Boolean
  | Integer
  | Decimal
  | Timestamp
  | Date
  | Time
  | Interval
  | ObjectId
  | Array
  | Object

derive instance eqEJsonType ∷ Eq EJsonType
derive instance ordEJsonType ∷ Ord EJsonType

instance showEJsonType ∷ Show EJsonType where
  show Null = "Null"
  show String = "String"
  show Boolean = "Boolean"
  show Integer = "Integer"
  show Decimal = "Decimal"
  show Timestamp = "Timestamp"
  show Date = "Date"
  show Time = "Time"
  show Interval = "Interval"
  show ObjectId = "ObjectId"
  show Array = "Array"
  show Object = "Object"
