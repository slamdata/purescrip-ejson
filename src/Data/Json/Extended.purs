module Data.Json.Extended
  ( EJsonF(..)
  , EJson(..)
  , getEJson

  , class EJsonView
  , intoEJson
  , outEJson

  , renderEJsonF
  , renderEJson

  , encodeEJsonF
  , encodeEJson

  , decodeEJsonF
  , decodeEJson

  , arbitraryEJsonF
  , arbitraryEJsonOfSize
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((>=>))

import Data.Eq1 (class Eq1)
import Data.Ord1 (class Ord1)

import Data.Argonaut.Core as JS
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Combinators ((.?))

import Data.Array as A
import Data.Either as E
import Data.Foldable as F
import Data.Functor.Mu as Mu
import Data.HugeNum as HN
import Data.Int as Int
import Data.List as L
import Data.Maybe as M
import Data.StrMap as SM
import Data.String.Regex as Rx
import Data.Traversable as TR
import Data.Tuple as T

import Test.StrongCheck as SC
import Test.StrongCheck.Gen as Gen

-- | The signature of the EJson theory.
data EJsonF a
  = Null
  | String String
  | Boolean Boolean
  | Integer Int
  | Decimal HN.HugeNum
  | String String
  | Timestamp String
  | Date String
  | Time String
  | Interval String
  | ObjectId String
  | Array (Array a)
  | OrderedSet (Array a)
  | Object (Array (T.Tuple a a))

instance functorEJsonF ∷ Functor EJsonF where
  map f x =
    case x of
      Null → Null
      String str → String str
      Boolean b → Boolean b
      Integer i → Integer i
      Decimal a → Decimal a
      Timestamp ts → Timestamp ts
      Date d → Date d
      Time t → Time t
      Interval i → Interval i
      ObjectId oid → ObjectId oid
      Array xs → Array $ f <$> xs
      OrderedSet xs → OrderedSet $ f <$> xs
      Object xs → Object $ bimapTuple f <$> xs
    where
      bimapTuple f (T.Tuple a b) =
        T.Tuple (f a) (f b)

instance eq1EJsonF ∷ Eq1 EJsonF where
  eq1 Null Null = true
  eq1 (Boolean b1) (Boolean b2) = b1 == b2
  eq1 (Integer i) (Integer j) = i == j
  eq1 (Decimal a) (Decimal b) = a == b
  eq1 (Integer i) (Decimal b) = HN.fromNumber (Int.toNumber i) == b
  eq1 (Decimal a) (Integer j) = a == HN.fromNumber (Int.toNumber j)
  eq1 (String a) (String b) = a == b
  eq1 (Timestamp a) (Timestamp b) = a == b
  eq1 (Date a) (Date b) = a == b
  eq1 (Time a) (Time b) = a == b
  eq1 (Interval a) (Interval b) = a == b
  eq1 (ObjectId a) (ObjectId b) = a == b
  eq1 (OrderedSet xs) (OrderedSet ys) = xs == ys
  eq1 (Array xs) (Array ys) = xs == ys
  eq1 (Object xs) (Object ys) = xs == ys
  eq1 _ _ = false

instance ord1EJsonF ∷ Ord1 EJsonF where
  compare1 Null Null = EQ
  compare1 _ Null = GT
  compare1 Null _ = LT

  compare1 (Boolean b1) (Boolean b2) = compare b1 b2
  compare1 _ (Boolean _) = GT
  compare1 (Boolean _) _ = LT

  compare1 (Integer i) (Integer j) = compare i j
  compare1 (Integer i) (Decimal b) = compare (HN.fromNumber (Int.toNumber i)) b
  compare1 (Decimal a) (Integer j) = compare a (HN.fromNumber (Int.toNumber j))
  compare1 _ (Integer _) = GT
  compare1 (Integer _) _ = LT

  compare1 (Decimal a) (Decimal b) = compare a b
  compare1 _ (Decimal _) = GT
  compare1 (Decimal _) _ = LT

  compare1 (String a) (String b) = compare a b
  compare1 _ (String _) = GT
  compare1 (String _) _ = LT

  compare1 (Timestamp a) (Timestamp b) = compare a b
  compare1 _ (Timestamp _) = GT
  compare1 (Timestamp _) _ = LT

  compare1 (Date a) (Date b) = compare a b
  compare1 _ (Date _) = GT
  compare1 (Date _) _ = LT

  compare1 (Time a) (Time b) = compare a b
  compare1 _ (Time _) = GT
  compare1 (Time _) _ = LT

  compare1 (Interval a) (Interval b) = compare a b
  compare1 _ (Interval _) = GT
  compare1 (Interval _) _ = LT

  compare1 (ObjectId a) (ObjectId b) = compare a b
  compare1 _ (ObjectId _) = GT
  compare1 (ObjectId _) _ = LT

  compare1 (OrderedSet a) (OrderedSet b) = compare a b
  compare1 _ (OrderedSet _) = GT
  compare1 (OrderedSet _) _ = LT

  compare1 (Array a) (Array b) = compare a b
  compare1 _ (Array _) = GT
  compare1 (Array _) _ = LT

  compare1 (Object a) (Object b) = compare a b


class EJsonView a where
  intoEJson ∷ EJsonF a → a
  outEJson ∷ a → E.Either a (EJsonF a)

newtype EJson = EJson (Mu.Mu EJsonF)

getEJson ∷ EJson → Mu.Mu EJsonF
getEJson (EJson x) = x

instance ejsonViewEJson ∷ EJsonView EJson where
  intoEJson = EJson <<< Mu.roll <<< map getEJson
  outEJson = getEJson >>> Mu.unroll >>> map EJson >>> E.Right

-- | This is a _lossy_ encoding of EJSON to JSON; JSON only supports objects with strings
-- as keys.
encodeEJson
  ∷ EJson
  → JS.Json
encodeEJson (EJson x) =
  encodeEJsonF
    encodeEJson
    (EJson <$> Mu.unroll x)

encodeEJsonF
  ∷ ∀ a
  . (EJsonView a)
  ⇒ (a → JS.Json)
  → EJsonF a
  → JS.Json
encodeEJsonF rec x =
  case x of
    Null → JS.jsonNull
    Boolean b → encodeJson b
    Integer i → encodeJson i
    Decimal a → encodeJson $ HN.toNumber a
    String str → encodeJson str
    Timestamp str → JS.jsonSingletonObject "$timestamp" $ encodeJson str
    Time str → JS.jsonSingletonObject "$time" $ encodeJson str
    Date str → JS.jsonSingletonObject "$date" $ encodeJson str
    Interval str → JS.jsonSingletonObject "$interval" $ encodeJson str
    ObjectId str → JS.jsonSingletonObject "$oid" $ encodeJson str
    OrderedSet xs → JS.jsonSingletonObject "$set" $ encodeJson $ rec <$> xs
    Array xs → encodeJson $ rec <$> xs
    Object xs → JS.jsonSingletonObject "$obj" $ encodeJson $ asStrMap xs
      where
        tuple
          ∷ T.Tuple a a
          → M.Maybe (T.Tuple String JS.Json)
        tuple (T.Tuple k v) =
          case outEJson k of
            E.Right (String str) → pure $ T.Tuple str (rec v)
            _ → M.Nothing

        asStrMap
          ∷ Array (T.Tuple a a)
          → SM.StrMap JS.Json
        asStrMap =
          SM.fromList
            <<< L.toList
            <<< A.mapMaybe tuple

decodeEJson
  ∷ ∀ a
  . (EJsonView a)
  ⇒ JS.Json
  → E.Either String a
decodeEJson x =
  map intoEJson $
    decodeEJsonF decodeEJson x

decodeEJsonF
  ∷ ∀ a
  . (EJsonView a)
  ⇒ (JS.Json → E.Either String a)
  → JS.Json
  → E.Either String (EJsonF a)
decodeEJsonF rec =
  JS.foldJson
    (\_ → pure Null)
    (pure <<< Boolean)
    (pure <<< decodeNumber)
    (pure <<< String)
    (map Array <<< TR.traverse rec)
    decodeObject

 where
    decodeNumber
      ∷ Number
      → EJsonF a
    decodeNumber a =
      case Int.fromNumber a of
        M.Just i → Integer i
        M.Nothing → Decimal $ HN.fromNumber a

    getOnlyKey
      ∷ String
      → JS.JObject
      → E.Either String JS.Json
    getOnlyKey key obj =
      case SM.keys obj of
        [_] → obj .? key
        keys → E.Left $ "Expected '" <> key <> "' to be the only key, but found: " <> show keys

    unwrapLeaf
      ∷ ∀ b
      . (DecodeJson b)
      ⇒ String
      → (b → EJsonF a)
      → JS.JObject
      → E.Either String (EJsonF a)
    unwrapLeaf key con =
      getOnlyKey key
        >=> decodeJson
        >>> map con

    unwrapBranch
      ∷ ∀ t
      . (TR.Traversable t, DecodeJson (t JS.Json))
      ⇒ String
      → (t a → EJsonF a)
      → JS.JObject
      → E.Either String (EJsonF a)
    unwrapBranch key con =
      getOnlyKey key
        >=> decodeJson
        >=> TR.traverse rec
        >>> map con

    unwrapNull
      ∷ JS.JObject
      → E.Either String (EJsonF a)
    unwrapNull =
      getOnlyKey "$na" >=>
        JS.foldJsonNull
          (E.Left "Expected null")
          (\_ → pure Null)

    decodeObject
      ∷ JS.JObject
      → E.Either String (EJsonF a)
    decodeObject obj =
      unwrapBranch "$obj" strMapObject obj
        <|> unwrapBranch "$set" OrderedSet obj
        <|> unwrapLeaf "$timestamp" Timestamp obj
        <|> unwrapLeaf "$date" Date obj
        <|> unwrapLeaf "$time" Time obj
        <|> unwrapLeaf "$interval" Interval obj
        <|> unwrapLeaf "$oid" ObjectId obj
        <|> unwrapNull obj
        <|> strMapObject <$> TR.traverse rec obj

    strMapObject
      ∷ SM.StrMap a
      → EJsonF a
    strMapObject =
      Object
        <<< L.fromList
        <<< map (\(T.Tuple k v) → T.Tuple (intoEJson $ String k) v)
        <<< SM.toList

arbitraryDecimal ∷ Gen.Gen HN.HugeNum
arbitraryDecimal =
  HN.fromNumber
    <<< Data.Int.toNumber
    <$> SC.arbitrary

arbitraryBaseEJsonF ∷ ∀ a. Gen.Gen (EJsonF a)
arbitraryBaseEJsonF =
  Gen.oneOf (pure Null)
    [ Boolean <$> SC.arbitrary
    , Integer <$> SC.arbitrary
    , Decimal <$> arbitraryDecimal
    , String <$> SC.arbitrary
    , Timestamp <$> SC.arbitrary
    , Date <$> SC.arbitrary
    , Time <$> SC.arbitrary
    , Interval <$> SC.arbitrary
    , ObjectId <$> SC.arbitrary
    , pure Null
    ]

arbitraryEJsonF
  ∷ ∀ a
  . Gen.Gen a
  → Gen.Gen (EJsonF a)
arbitraryEJsonF rec =
  Gen.oneOf (pure Null)
    [ arbitraryBaseEJsonF
    , OrderedSet <$> Gen.arrayOf rec
    , Array <$> Gen.arrayOf rec
    , Object <$> Gen.arrayOf arbitraryTuple
    ]

  where
    arbitraryTuple ∷ Gen.Gen (T.Tuple a a)
    arbitraryTuple =
      T.Tuple
        <$> rec
        <*> rec

arbitraryEJsonOfSize
  ∷ ∀ a
  . (EJsonView a)
  ⇒ Gen.Size
  → Gen.Gen a
arbitraryEJsonOfSize size =
  intoEJson <$>
    case size of
      0 → arbitraryBaseEJsonF
      n → arbitraryEJsonF $ arbitraryEJsonOfSize (n - 1)

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

renderEJson
  ∷ EJson
  → String
renderEJson (EJson x) =
  renderEJsonF
    renderEJson
    (EJson <$> Mu.unroll x)


hole ∷ ∀ a. a
hole = Unsafe.Coerce.unsafeCoerce "a"

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

