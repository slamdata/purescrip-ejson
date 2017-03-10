module Data.Json.Extended.Signature.Render
  ( renderEJsonF
  , renderTimestamp
  , renderDate
  , renderTime
  ) where

import Prelude

import Data.DateTime as DT
import Data.Either (fromRight)
import Data.Enum (class BoundedEnum, fromEnum)
import Data.Foldable as F
import Data.HugeNum as HN
import Data.Json.Extended.Signature.Core (EJsonF(..))
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.Tuple as T

import Matryoshka (Algebra)

import Partial.Unsafe (unsafePartial)

renderEJsonF ∷ Algebra EJsonF String
renderEJsonF = case _ of
  Null → "null"
  Boolean b → if b then "true" else "false"
  Integer i → show i
  Decimal a → HN.toString a
  String str → stringEJson str
  Timestamp dt → tagged "TIMESTAMP" (renderTimestamp dt)
  Time t → tagged "TIME" (renderTime t)
  Date d → tagged "DATE" (renderDate d)
  Interval str → tagged "INTERVAL" str
  ObjectId str → tagged "OID" str
  Array ds → squares $ commaSep ds
  Map ds → braces $ renderPairs ds

tagged ∷ String → String → String
tagged tag str =
  tag <> parens (stringEJson str)

replaceAll ∷ String → String → String → String
replaceAll i =
  RX.replace $ unsafePartial fromRight $ RX.regex i RXF.global

  -- | Surround text in double quotes, escaping internal double quotes.
stringEJson ∷ String → String
stringEJson str =
  "\"" <> replaceAll "\"" "\\\"" str <> "\""

commaSep ∷ Array String → String
commaSep = F.intercalate ", "

renderPairs ∷ Array (T.Tuple String String) → String
renderPairs = commaSep <<< map (\(T.Tuple k v) → k <> ": " <> v)

renderTimestamp ∷ DT.DateTime → String
renderTimestamp dt =
  renderDate (DT.date dt) <> "T" <> renderTime (DT.time dt) <> "Z"

renderDate ∷ DT.Date → String
renderDate d =
  show1000 (DT.year d) <> "-" <> show10 (DT.month d) <> "-" <> show10 (DT.day d)

renderTime ∷ DT.Time → String
renderTime t =
  show10 (DT.hour t) <> ":" <> show10 (DT.minute t) <> ":" <> show10 (DT.second t)

show1000 ∷ ∀ e. BoundedEnum e ⇒ e → String
show1000 c =
  case fromEnum c of
    n | n < 10 → "000" <> show n
      | n < 100 → "00" <> show n
      | n < 1000 → "0" <> show n
      | otherwise → show n

show10 ∷ ∀ e. BoundedEnum e ⇒ e → String
show10 c =
  let n = fromEnum c
  in if n < 10 then "0" <> show n else show n

parens ∷ String → String
parens str = "(" <> str <> ")"

squares ∷ String → String
squares str = "[" <> str <> "]"

braces ∷ String → String
braces str = "{" <> str <> "}"
