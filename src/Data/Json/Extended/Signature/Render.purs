module Data.Json.Extended.Signature.Render
  ( renderEJsonF
  ) where

import Prelude

import Data.Either (fromRight)
import Data.Foldable as F
import Data.HugeNum as HN
import Data.Json.Extended.Signature.Core (EJsonF(..), EJsonMap(..))
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
  Array ds → squares $ commaSep ds
  Map (EJsonMap ds) → braces $ renderPairs ds

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

parens ∷ String → String
parens str = "(" <> str <> ")"

squares ∷ String → String
squares str = "[" <> str <> "]"

braces ∷ String → String
braces str = "{" <> str <> "}"
