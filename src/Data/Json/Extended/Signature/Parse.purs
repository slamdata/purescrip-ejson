module Data.Json.Extended.Signature.Parse
  ( parseEJsonF
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))
import Data.Functor ((<$), ($>))

import Data.Array as A
import Data.Foldable as F
import Data.HugeNum as HN
import Data.Int as Int
import Data.List as L
import Data.Maybe as M
import Data.String as S
import Data.Tuple as T

import Data.Json.Extended.Signature.Core (EJsonF(..))

import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS

parens
  ∷ ∀ m a
  . (Monad m)
  ⇒ P.ParserT String m a
  → P.ParserT String m a
parens =
  PC.between
    (PS.string "(")
    (PS.string ")")

squares
  ∷ ∀ m a
  . (Monad m)
  ⇒ P.ParserT String m a
  → P.ParserT String m a
squares =
  PC.between
    (PS.string "[")
    (PS.string "]")

braces
  ∷ ∀ m a
  . (Monad m)
  ⇒ P.ParserT String m a
  → P.ParserT String m a
braces =
  PC.between
    (PS.string "{")
    (PS.string "}")

commaSep
  ∷ ∀ m a
  . (Monad m)
  ⇒ P.ParserT String m a
  → P.ParserT String m (L.List a)
commaSep =
  flip PC.sepBy $
    PS.skipSpaces
      *> PS.string ","
      <* PS.skipSpaces

stringLiteral
  ∷ ∀ m
  . (Monad m)
  ⇒ P.ParserT String m String
stringLiteral =
  PC.between quote quote (A.many stringChar)
    <#> S.fromCharArray

  where
    quote = PS.string "\""

    stringChar =
      PC.try stringEscape
        <|> stringLetter

    stringLetter =
      PS.satisfy \c →
        c /= '"'

    stringEscape =
      PS.string "\\\"" $> '"'

taggedLiteral
  ∷ ∀ m
  . (Monad m)
  ⇒ String
  → P.ParserT String m String
taggedLiteral tag =
  PC.try $
    PS.string tag
      *> parens stringLiteral

anyString
  ∷ ∀ m
  . (Monad m)
  ⇒ P.ParserT String m String
anyString =
  A.many PS.anyChar
    <#> S.fromCharArray

parseBoolean
  ∷ ∀ m
  . (Monad m)
  ⇒ P.ParserT String m Boolean
parseBoolean =
  PC.choice
    [ true <$ PS.string "true"
    , false <$ PS.string "false"
    ]

parseDigit
  ∷ ∀ m
   . (Monad m)
  ⇒ P.ParserT String m Int
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

many1
  ∷ ∀ m s a
  . (Monad m)
  ⇒ P.ParserT s m a
  → P.ParserT s m (L.List a)
many1 p =
  L.Cons
    <$> p
    <*> L.many p

parseNat
  ∷ ∀ m
  . (Monad m)
  ⇒ P.ParserT String m Int
parseNat =
  many1 parseDigit
    <#> F.foldl (\a i → a * 10 + i) 0

parseNegative
  ∷ ∀ m a
  . (Monad m, Ring a)
  ⇒ P.ParserT String m a
  → P.ParserT String m a
parseNegative p =
  PS.string "-"
    *> PS.skipSpaces
    *> p
    <#> negate

parsePositive
  ∷ ∀ m a
  . (Monad m, Ring a)
  ⇒ P.ParserT String m a
  → P.ParserT String m a
parsePositive p =
  PC.optional (PS.string "+" *> PS.skipSpaces)
    *> p

parseSigned
  ∷ ∀ m a
  . (Monad m, Ring a)
  ⇒ P.ParserT String m a
  → P.ParserT String m a
parseSigned p =
  parseNegative p
    <|> parsePositive p

parseInt
  ∷ ∀ m
  . (Monad m)
  ⇒ P.ParserT String m Int
parseInt =
  parseSigned parseNat

parseExponent
  ∷ ∀ m
  . (Monad m)
  ⇒ P.ParserT String m Int
parseExponent =
  (PS.string "e" <|> PS.string "E")
    *> parseInt

parsePositiveScientific
  ∷ ∀ m
  . (Monad m)
  ⇒ P.ParserT String m HN.HugeNum
parsePositiveScientific = do
  let ten = HN.fromNumber 10.0
  lhs ← PC.try $ fromInt <$> parseNat <* PS.string "."
  rhs ← A.many parseDigit <#> F.foldr (\d f → divNum (f + fromInt d) ten) zero
  exp ← parseExponent
  pure $ (lhs + rhs) * safePow ten exp

  where
    fromInt = HN.fromNumber <<< Int.toNumber

    -- TODO: remove when HugeNum adds division
    divNum a b =
      HN.fromNumber $
        HN.toNumber a / HN.toNumber b

    -- To work around: https://github.com/Thimoteus/purescript-hugenums/issues/6
    safePow a 0 = one
    safePow a n = HN.pow a n

parseHugeNum
  ∷ ∀ m
  . (Monad m)
  ⇒ P.ParserT String m HN.HugeNum
parseHugeNum = do
  chars ← A.many (PS.oneOf ['0','1','2','3','4','5','6','7','8','9','-','.']) <#> S.fromCharArray
  case HN.fromString chars of
    M.Just num → pure num
    M.Nothing → P.fail $ "Failed to parse decimal: " <> chars

parseScientific
  ∷ ∀ m
  . (Monad m)
  ⇒ P.ParserT String m HN.HugeNum
parseScientific =
  parseSigned parsePositiveScientific

parseDecimal
  ∷ ∀ m
  . (Monad m)
  ⇒ P.ParserT String m HN.HugeNum
parseDecimal =
  parseHugeNum
    <|> parseScientific

-- | Parse one layer of structure.
parseEJsonF
  ∷ ∀ m a
  . (Monad m)
  ⇒ P.ParserT String m a
  → P.ParserT String m (EJsonF a)
parseEJsonF rec =
  PC.choice $
    [ Null <$ PS.string "null"
    , Boolean <$> parseBoolean
    , Decimal <$> PC.try parseDecimal
    , Integer <$> parseInt
    , String <$> stringLiteral
    , Timestamp <$> taggedLiteral "TIMESTAMP"
    , Time <$> taggedLiteral "TIME"
    , Date <$> taggedLiteral "DATE"
    , Interval <$> taggedLiteral "INTERVAL"
    , ObjectId <$> taggedLiteral "OID"
    , Array <<< L.fromList <$> squares (commaSep rec)
    , Object <<< L.fromList <$> braces (commaSep parseAssignment)
    ]

  where
    parseColon ∷ P.ParserT String m String
    parseColon =
      PS.skipSpaces
        *> PS.string ":"
        <* PS.skipSpaces

    parseAssignment ∷ P.ParserT String m (T.Tuple a a)
    parseAssignment =
      T.Tuple
        <$> rec <* parseColon
        <*> rec

