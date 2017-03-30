module Data.Json.Extended.Signature.Parse
  ( parseEJsonF
  , parseNull
  , parseBooleanLiteral
  , parseDecimalLiteral
  , parseIntLiteral
  , parseStringLiteral
  , parseTimestampLiteral
  , parseTimestamp
  , parseTimeLiteral
  , parseTime
  , parseDateLiteral
  , parseDate
  , parseIntervalLiteral
  , parseObjectIdLiteral
  , parseArrayLiteral
  , parseMapLiteral
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.State (get)

import Data.Array as A
import Data.DateTime as DT
import Data.Either as E
import Data.Enum (toEnum)
import Data.Foldable as F
import Data.HugeNum as HN
import Data.Int as Int
import Data.Json.Extended.Signature.Core (EJsonF(..), EJsonMap(..))
import Data.List as L
import Data.Maybe as M
import Data.String as S
import Data.Tuple as T

import Matryoshka (CoalgebraM)

import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS

parens
  ∷ ∀ m a
  . Monad m
  ⇒ P.ParserT String m a
  → P.ParserT String m a
parens =
  PC.between
    (PS.string "(")
    (PS.string ")")

squares
  ∷ ∀ m a
  . Monad m
  ⇒ P.ParserT String m a
  → P.ParserT String m a
squares =
  PC.between
    (PS.string "[")
    (PS.string "]")

braces
  ∷ ∀ m a
  . Monad m
  ⇒ P.ParserT String m a
  → P.ParserT String m a
braces =
  PC.between
    (PS.string "{")
    (PS.string "}")

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

taggedLiteral
  ∷ ∀ m a
  . Monad m
  ⇒ String
  → (P.ParserT String m a)
  → P.ParserT String m a
taggedLiteral tag p =
  PC.try $
    PS.string tag
      *> parens (quoted p)

anyTagged ∷ ∀ m. Monad m ⇒ P.ParserT String m String
anyTagged = do
  tag ← map S.fromCharArray $ A.many tagLetter
  val ← parens $ quoted stringInner
  pure $ tag <> "(\"" <> val <> "\")"
  where
  tagLetter = PS.satisfy \a → a /= '"' && a /= '(' && a /= ':'
--  stringEscape = PS.string "\\\"" $> '"'

-- | Parses time _values_ of the form `HH:mm:SS`. For the EJson time literal
-- | `TIME("HH:mm:SS")` use `parseTimeLiteral`.
parseTime ∷ ∀ m. Monad m ⇒ P.ParserT String m DT.Time
parseTime = do
  hour ← parse10
  PS.string ":"
  minute ← parse10
  PS.string ":"
  second ← parse10
  case DT.Time <$> toEnum hour <*> toEnum minute <*> toEnum second <*> pure bottom of
    M.Just dt → pure dt
    M.Nothing →
      P.fail $ "Invalid time value " <> show hour <> ":" <> show minute <> ":" <> show second

-- | Parses date _values_ of the form `YYYY-MM-DD`. For the EJson date literal
-- | `DATE("YYYY-MM-DD")` use `parseDateLiteral`.
parseDate ∷ ∀ m. Monad m ⇒ P.ParserT String m DT.Date
parseDate = do
  year ← parse1000
  PS.string "-"
  month ← parse10
  PS.string "-"
  day ← parse10
  case join $ DT.exactDate <$> toEnum year <*> toEnum month <*> toEnum day of
    M.Just dt → pure dt
    M.Nothing →
      P.fail $ "Invalid date value " <> show year <> "-" <> show month <> "-" <> show day

-- | Parses timestamp _values_ of the form `YYYY-MM-DDTHH:mm:SSZ`. For the
-- | EJson timestamp literal `TIMESTAMP("YYYY-MM-DDTHH:mm:SSZ")` use
-- | `parseTimestampLiteral`.
parseTimestamp ∷ ∀ m. Monad m ⇒ P.ParserT String m DT.DateTime
parseTimestamp = do
  d ← parseDate
  PS.string "T"
  t ← parseTime
  PS.string "Z"
  pure $ DT.DateTime d t

anyString
  ∷ ∀ m
  . Monad m
  ⇒ P.ParserT String m String
anyString =
  map S.fromCharArray $ A.many PS.anyChar

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

many1
  ∷ ∀ m s a
  . Monad m
  ⇒ P.ParserT s m a
  → P.ParserT s m (L.List a)
many1 p =
  L.Cons
    <$> p
    <*> L.many p

parseNat
  ∷ ∀ m
  . Monad m
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

parseTimestampLiteral ∷ ∀ m. Monad m ⇒ P.ParserT String m DT.DateTime
parseTimestampLiteral = taggedLiteral "TIMESTAMP" parseTimestamp

parseTimeLiteral ∷ ∀ m. Monad m ⇒ P.ParserT String m DT.Time
parseTimeLiteral = taggedLiteral "TIME" parseTime

parseDateLiteral ∷ ∀ m. Monad m ⇒ P.ParserT String m DT.Date
parseDateLiteral = taggedLiteral "DATE" parseDate

parseIntervalLiteral ∷ ∀ m. Monad m ⇒ P.ParserT String m String
parseIntervalLiteral = taggedLiteral "INTERVAL" stringInner

parseObjectIdLiteral ∷ ∀ m. Monad m ⇒ P.ParserT String m String
parseObjectIdLiteral = taggedLiteral "OID" stringInner

parseArrayLiteral
  ∷ ∀ m
  . Monad m
  ⇒ P.ParserT String m (Array String)
parseArrayLiteral = do
  (P.ParseState s _ _) ← get
  let
    err ∷ ∀ a. E.Either String a
    err = E.Left "Incorrect Array literal"

    start =
      { acc: []
      , squares: 0
      , brackets: 0
      , quotes: false
      , slashed: false
      , result: [ ]
      }
    foldFn acc ch = case ch of
      '[' → pure acc{ acc = A.cons ch acc.acc
                    , squares = acc.squares + one
                    }
      ']' | acc.squares < 0 → err
          | otherwise → pure acc{ acc = A.cons ch acc.acc
                                , squares = acc.squares - one
                                }
      '{' → pure acc{ acc = A.cons ch acc.acc
                    , brackets = acc.brackets + one
                    }
      '}' | acc.brackets < 0 → err
          | otherwise → pure acc{ acc = A.cons ch acc.acc
                                , brackets = acc.brackets - one
                                }
      '"' → pure acc{ acc = A.cons ch acc.acc
                    , quotes = if acc.slashed then acc.quotes else not acc.quotes
                    }
      '\\' → pure acc{ acc = A.cons ch acc.acc
                     , slashed = not acc.slashed
                     }
      ',' | acc.quotes → pure acc { acc = A.cons ch acc.acc }
          | acc.brackets /= 0
            || acc.squares /= 0
            || acc.quotes
            || acc.slashed → err
          | otherwise → pure acc { acc = [ ]
                                 , result =
                                      A.snoc acc.result
                                      $ S.trim
                                      $ S.fromCharArray
                                      $ A.reverse acc.acc
                                 }
      ':' | not acc.quotes && acc.brackets < 1 → err
          | otherwise → pure acc { acc = A.cons ch acc.acc }
      _ → pure acc { acc = A.cons ch acc.acc }

    result ∷ E.Either String (Array String)
    result =
      case S.stripSuffix (S.Pattern "]") s >>= S.stripPrefix (S.Pattern "[") of
        M.Nothing → err
        M.Just str → do
          folded ← A.foldRecM foldFn start $ S.toCharArray str
          if (folded.quotes
            || folded.slashed
            || folded.squares /= 0
            || folded.brackets /= 0)
            then err
            else
            pure
            $ A.filter (_ /= "")
            $ A.snoc folded.result
            $ S.trim
            $ S.fromCharArray
            $ A.reverse folded.acc

  case result of
    E.Left e → P.fail e
    E.Right r → pure r


parseMapLiteral
  ∷ ∀ m
  . Monad m
  ⇒ P.ParserT String m (EJsonMap String)
parseMapLiteral = do
  (P.ParseState s _ _) ← get
  let
    err ∷ ∀ a. E.Either String a
    err = E.Left "Incorrect Map literal"

    start =
      { acc: []
      , squares: 0
      , brackets: 0
      , quotes: false
      , slashed: false
      , fst: ""
      , result: [ ]
      }
    foldFn acc ch = case ch of
      '[' → pure acc{ acc = A.cons ch acc.acc
                    , squares = acc.squares + one
                    }
      ']' | acc.squares < 0 → err
          | otherwise → pure acc{ acc = A.cons ch acc.acc
                                , squares = acc.squares - one
                                }
      '{' → pure acc{ acc = A.cons ch acc.acc
                    , brackets = acc.brackets + one
                    }
      '}' | acc.brackets < 0 → err
          | otherwise → pure acc{ acc = A.cons ch acc.acc
                                , brackets = acc.brackets - one
                                }
      '"' → pure acc{ acc = A.cons ch acc.acc
                    , quotes = if acc.slashed then acc.quotes else not acc.quotes
                    }
      '\\' → pure acc{ acc = A.cons ch acc.acc
                     , slashed = not acc.slashed
                     }
      ',' | acc.quotes → pure acc { acc = A.cons ch acc.acc }
          | acc.brackets /= 0
            || acc.squares /= 0
            || acc.quotes
            || acc.slashed → err
          | acc.fst == "" → err
          | otherwise → pure acc { acc = [ ]
                                 , result =
                                      A.snoc acc.result
                                      $ T.Tuple acc.fst
                                      $ S.trim
                                      $ S.fromCharArray
                                      $ A.reverse acc.acc
                                 , fst = ""
                                 }
      ':' | not acc.quotes && acc.brackets < 0 → err
          | not acc.quotes && acc.brackets == 0 →
              pure acc { fst = S.trim $ S.fromCharArray $ A.reverse acc.acc
                       , acc = [ ]
                       }
          | otherwise → pure acc { acc = A.cons ch acc.acc }
      _ → pure acc { acc = A.cons ch acc.acc }

    result ∷ E.Either String (EJsonMap String)
    result =
      case S.stripSuffix (S.Pattern "}") s >>= S.stripPrefix (S.Pattern "{") of
        M.Nothing → err
        M.Just str → do
          folded ← A.foldRecM foldFn start $ S.toCharArray str
          if (folded.quotes
            || folded.slashed
            || folded.squares /= 0
            || folded.brackets /= 0
            )
            then err
            else
            pure
            $ EJsonMap
            $ A.filter (\(T.Tuple k v) → k /= "")
            $ A.snoc folded.result
            $ T.Tuple folded.fst
            $ S.trim
            $ S.fromCharArray
            $ A.reverse folded.acc

  case result of
    E.Left e → P.fail e
    E.Right r → pure r


parseEJsonF ∷ CoalgebraM (E.Either P.ParseError) EJsonF String
parseEJsonF s =
  s # S.trim # P.runParser
  $ PC.choice
    [ Null <$ parseNull
    , Boolean <$> parseBooleanLiteral
    , Decimal <$> PC.try parseDecimalLiteral
    , Integer <$> parseIntLiteral
    , Timestamp <$> parseTimestampLiteral
    , Time <$> parseTimeLiteral
    , Date <$> parseDateLiteral
    , Interval <$> parseIntervalLiteral
    , ObjectId <$> parseObjectIdLiteral
    , String <$> parseStringLiteral
    , Array <$> parseArrayLiteral
    , Map <$> parseMapLiteral
    , P.fail "Incorrect EJson"
    ]
