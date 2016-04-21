module Data.Json.Extended.Signature.Render
  ( renderEJsonF
  ) where

import Prelude

import Data.Foldable as F
import Data.HugeNum as HN
import Data.String.Regex as Rx
import Data.Tuple as T

import Data.Json.Extended.Signature.Core (EJsonF(..))

renderEJsonF
  ∷ ∀ a
  . (a → String)
  → EJsonF a
  → String
renderEJsonF rec d =
  case d of
    Null → "null"
    Boolean b → if b then "true" else "false"
    Integer i → show i
    Decimal a → HN.toString a
    String str → stringEJson str
    Timestamp str → tagged "TIMESTAMP" str
    Time str → tagged "TIME" str
    Date str → tagged "DATE" str
    Interval str → tagged "INTERVAL" str
    ObjectId str → tagged "OID" str
    OrderedSet ds → parens $ commaSep ds
    Array ds → squares $ commaSep ds
    Object ds → braces $ renderPairs ds
  where
    tagged
      ∷ String
      → String
      → String
    tagged tag str =
      tag <> parens (stringEJson str)

    replaceAll
      ∷ String
      → String
      → String
      → String
    replaceAll i =
      Rx.replace $
        Rx.regex i $
          Rx.noFlags { global = true }

    -- | Surround text in double quotes, escaping internal double quotes.
    stringEJson
      ∷ String
      → String
    stringEJson str =
      "\"" <> replaceAll "\"" "\\\"" str <> "\""

    commaSep
      ∷ ∀ f
      . (Functor f, F.Foldable f)
      ⇒ f a
      → String
    commaSep =
      F.intercalate "," <<<
        map rec

    renderPairs
      ∷ Array (T.Tuple a a)
      → String
    renderPairs =
      F.intercalate ", " <<<
        map \(T.Tuple k v) →
          rec k <> ": " <> rec v

parens
  ∷ String
  → String
parens str =
  "(" <> str <> ")"

squares
  ∷ String
  → String
squares str =
  "[" <> str <> "]"

braces
  ∷ String
  → String
braces str =
  "{" <> str <> "}"

