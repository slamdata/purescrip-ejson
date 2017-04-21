module Data.Json.Extended.Signature.Parse
  ( parseEJsonF
  , parseNull
  , parseBooleanLiteral
  , parseDecimalLiteral
  , parseIntLiteral
  , parseStringLiteral
  , parseArrayLiteral
  , parseMapLiteral
  ) where

import Prelude

import Control.Alt ((<|>))

import Data.Array as A
import Data.Foldable as F
import Data.HugeNum as HN
import Data.Int as Int
import Data.Json.Extended.Signature.Core (EJsonF(..), EJsonMap(..))
import Data.List as L
import Data.Maybe as M
import Data.String as S
import Data.Tuple as T

import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS

squares
  ∷ ∀ m a
  . Monad m
  ⇒ P.ParserT String m a
  → P.ParserT String m a
squares =
  PC.between
    (PS.string "[" *> PS.skipSpaces)
    (PS.skipSpaces *> PS.string "]")

braces
  ∷ ∀ m a
  . Monad m
  ⇒ P.ParserT String m a
  → P.ParserT String m a
braces =
  PC.between
    (PS.string "{")
    (PS.string "}")

commaSep
  ∷ ∀ m a
  . Monad m
  ⇒ P.ParserT String m a
  → P.ParserT String m (L.List a)
commaSep p = do
  PS.skipSpaces
  o ← PC.sepBy p do
    PS.skipSpaces
    _ ← PS.string ","
    PS.skipSpaces
  PS.skipSpaces
  pure o

stringInner ∷ ∀ m . Monad m ⇒ P.ParserT String m String
stringInner = A.many stringChar <#> S.fromCharArray
  where
  stringChar = PC.try stringEscape <|> stringLetter
  stringLetter = PS.satisfy (_ /= '"')
  stringEscape = PS.string "\\\"" $> '"'

quoted ∷ ∀ a m. Monad m ⇒ P.ParserT String m a → P.ParserT String m a
quoted = PC.between quote quote
  where
  quote = PS.string "\""

parseDigit ∷ ∀ m. Monad m ⇒ P.ParserT String m Int
parseDigit =
  PC.choice
    [ 0 <$ PS.string "0"
    , 1 <$ PS.string "1"
    , 2 <$ PS.string "2"
    , 3 <$ PS.string "3"
    , 4 <$ PS.string "4"
    , 5 <$ PS.string "5"
    , 6 <$ PS.string "6"
    , 7 <$ PS.string "7"
    , 8 <$ PS.string "8"
    , 9 <$ PS.string "9"
    ]

parse10 ∷ ∀ m. Monad m ⇒ P.ParserT String m Int
parse10 = (tens <$> parseDigit <*> parseDigit) <|> parseDigit
  where
  tens x y = x * 10 + y

parse1000 ∷ ∀ m. Monad m ⇒ P.ParserT String m Int
parse1000
  = (thousands <$> parseDigit <*> parseDigit <*> parseDigit <*> parseDigit)
  <|> (hundreds <$> parseDigit <*> parseDigit <*> parseDigit)
  <|> (tens <$> parseDigit <*> parseDigit)
  <|> parseDigit
  where
  thousands x y z w = x * 1000 + y * 100 + z * 10 + w
  hundreds x y z = x * 100 + y * 10 + z
  tens x y = x * 10 + y

parseNat
  ∷ ∀ m
  . Monad m
  ⇒ P.ParserT String m Int
parseNat =
  A.some parseDigit
    <#> F.foldl (\a i → a * 10 + i) 0

parseNegative
  ∷ ∀ m a
  . Monad m
  ⇒ Ring a
  ⇒ P.ParserT String m a
  → P.ParserT String m a
parseNegative p =
  PS.string "-"
    *> PS.skipSpaces
    *> p
    <#> negate

parsePositive
  ∷ ∀ m a
  . Monad m
  ⇒ Ring a
  ⇒ P.ParserT String m a
  → P.ParserT String m a
parsePositive p =
  PC.optional (PS.string "+" *> PS.skipSpaces)
    *> p

parseSigned
  ∷ ∀ m a
  . Monad m
  ⇒ Ring a
  ⇒ P.ParserT String m a
  → P.ParserT String m a
parseSigned p =
  parseNegative p
    <|> parsePositive p

parseExponent
  ∷ ∀ m
  . Monad m
  ⇒ P.ParserT String m Int
parseExponent =
  (PS.string "e" <|> PS.string "E")
    *> parseIntLiteral

parsePositiveScientific
  ∷ ∀ m
  . Monad m
  ⇒ P.ParserT String m HN.HugeNum
parsePositiveScientific = do
  let ten = HN.fromNumber 10.0
  lhs ← PC.try $ fromInt <$> parseNat <* PS.string "."
  rhs ← A.many parseDigit <#> F.foldr (\d f → divNum (f + fromInt d) ten) zero
  exp ← parseExponent
  pure $ (lhs + rhs) * HN.pow ten exp

  where
  fromInt = HN.fromNumber <<< Int.toNumber

  -- TODO: remove when HugeNum adds division
  divNum a b =
    HN.fromNumber $
    HN.toNumber a / HN.toNumber b


parseHugeNum
  ∷ ∀ m
  . Monad m
  ⇒ P.ParserT String m HN.HugeNum
parseHugeNum = do
  chars ← A.many (PS.oneOf ['0','1','2','3','4','5','6','7','8','9','-','.']) <#> S.fromCharArray
  case HN.fromString chars of
    M.Just num → pure num
    M.Nothing → P.fail $ "Failed to parse decimal: " <> chars

parseScientific
  ∷ ∀ m
  . Monad m
  ⇒ P.ParserT String m HN.HugeNum
parseScientific =
  parseSigned parsePositiveScientific

parseNull ∷ ∀ m. Monad m ⇒ P.ParserT String m Unit
parseNull = PS.string "null" $> unit

parseBooleanLiteral ∷ ∀ m. Monad m ⇒ P.ParserT String m Boolean
parseBooleanLiteral =
  PC.choice
    [ true <$ PS.string "true"
    , false <$ PS.string "false"
    ]

parseDecimalLiteral ∷ ∀ m. Monad m ⇒ P.ParserT String m HN.HugeNum
parseDecimalLiteral = parseHugeNum <|> parseScientific

parseIntLiteral ∷ ∀ m. Monad m ⇒ P.ParserT String m Int
parseIntLiteral = parseSigned parseNat

parseStringLiteral ∷ ∀ m. Monad m ⇒ P.ParserT String m String
parseStringLiteral = quoted stringInner

parseArrayLiteral :: forall a m. Monad m => P.ParserT String m a -> P.ParserT String m (Array a)
parseArrayLiteral p = A.fromFoldable <$> squares (commaSep p)

parseMapLiteral :: forall a m. Monad m => P.ParserT String m a -> P.ParserT String m (EJsonMap a)
parseMapLiteral p = EJsonMap <<< A.fromFoldable <$> braces (commaSep parseAssignment)
  where
  parseColon ∷ P.ParserT String m String
  parseColon = PS.skipSpaces *> PS.string ":" <* PS.skipSpaces
  parseAssignment ∷ P.ParserT String m (T.Tuple a a)
  parseAssignment = T.Tuple <$> p <* parseColon <*> p

-- | Parse one layer of structure.
parseEJsonF
  ∷ ∀ m a
  . Monad m
  ⇒ P.ParserT String m a
  → P.ParserT String m (EJsonF a)
parseEJsonF rec =
  PC.choice $
    [ Null <$ parseNull
    , Boolean <$> parseBooleanLiteral
    , Decimal <$> PC.try parseDecimalLiteral
    , Integer <$> parseIntLiteral
    , String <$> parseStringLiteral
    , Array <$> parseArrayLiteral rec
    , Map <$> parseMapLiteral rec
    ]
