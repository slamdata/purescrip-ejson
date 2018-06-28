module Data.Json.Extended.Signature.Render
  ( renderEJsonF
  ) where

import Prelude

import Data.Foldable as F
import Data.HugeInt as HI
import Data.HugeNum as HN
import Data.Json.Extended.Signature.Core (EJsonF(..), EJsonMap(..))
import Data.String.Pattern (Pattern(..), Replacement(..)) as Str
import Data.String.Common (replaceAll) as Str
import Data.String.CodeUnits (takeWhile) as Str
import Data.Tuple as T
import Matryoshka (Algebra)

renderEJsonF ∷ Algebra EJsonF String
renderEJsonF = case _ of
  Null → "null"
  Boolean b → if b then "true" else "false"
  Integer i → Str.takeWhile (_ /= '.') $ HN.toString $ HI.toHugeNum i
  Decimal a → HN.toString a
  String str → stringEJson str
  Array ds → squares $ commaSep ds
  Map (EJsonMap ds) → braces $ renderPairs ds

  -- | Surround text in double quotes, escaping internal double quotes.
stringEJson ∷ String → String
stringEJson str =
  "\"" <> escaped <> "\""
  where
  escaped =
    str
      # Str.replaceAll (Str.Pattern "\\") (Str.Replacement "\\\\")
      # Str.replaceAll (Str.Pattern "\t") (Str.Replacement "\\t")
      # Str.replaceAll (Str.Pattern "\r") (Str.Replacement "\\r")
      # Str.replaceAll (Str.Pattern "\n") (Str.Replacement "\\n")
      # Str.replaceAll (Str.Pattern "\"") (Str.Replacement "\\\"")

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
