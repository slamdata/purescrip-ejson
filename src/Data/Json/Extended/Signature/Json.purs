module Data.Json.Extended.Signature.Json where

import Prelude

import Control.Alt ((<|>))

import Data.Argonaut.Core as JS
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (encodeJson)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.DateTime as DT
import Data.Either as E
import Data.HugeNum as HN
import Data.Int as Int
import Data.Json.Extended.Signature.Core (EJsonF(..), EJsonMap(..))
import Data.Json.Extended.Signature.Parse (parseDate, parseTime, parseTimestamp)
import Data.Json.Extended.Signature.Render (renderDate, renderTime, renderTimestamp)
import Data.Maybe as M
import Data.StrMap as SM
import Data.Traversable as TR
import Text.Parsing.Parser as P

import Matryoshka (Algebra, CoalgebraM)

encodeJsonEJsonF ∷ Algebra EJsonF JS.Json
encodeJsonEJsonF = case _ of
  Null → JS.jsonNull
  Boolean b → encodeJson b
  Integer i → encodeJson i
  Decimal a → encodeJson $ HN.toNumber a
  String str → encodeJson str
  Timestamp dt → JS.jsonSingletonObject "$timestamp" $ encodeJson $ renderTimestamp dt
  Time t → JS.jsonSingletonObject "$time" $ encodeJson $ renderTime t
  Date d → JS.jsonSingletonObject "$date" $ encodeJson $ renderDate d
  Interval str → JS.jsonSingletonObject "$interval" $ encodeJson str
  ObjectId str → JS.jsonSingletonObject "$oid" $ encodeJson str
  Array xs → encodeJson xs
  Map (EJsonMap xs) → JS.jsonSingletonObject "$obj" $ encodeJson xs

decodeJsonEJsonF ∷ CoalgebraM (E.Either String) EJsonF JS.Json
decodeJsonEJsonF =
  JS.foldJson
    (\_ → E.Right Null)
    (E.Right <<< Boolean)
    (E.Right <<< decodeNumber)
    (E.Right <<< String)
    decodeArray
    decodeObject
  where
  decodeNumber ∷ Number → EJsonF JS.Json
  decodeNumber a = case Int.fromNumber a of
    M.Just i → Integer i
    M.Nothing → Decimal $ HN.fromNumber a

  decodeArray ∷ JS.JArray → E.Either String (EJsonF JS.Json)
  decodeArray arr = E.Right $ Array arr

  decodeObject
    ∷ JS.JObject
    → E.Either String (EJsonF JS.Json)
  decodeObject obj =
    unwrapBranch "$obj" strMapObject obj
    <|> unwrapBranch "$obj" arrTpls obj
    <|> unwrapLeaf "$timestamp" decodeTimestamp Timestamp obj
    <|> unwrapLeaf "$date" decodeDate Date obj
    <|> unwrapLeaf "$time" decodeTime Time obj
    <|> unwrapLeaf "$interval" decodeJson Interval obj
    <|> unwrapLeaf "$oid" decodeJson ObjectId obj
    <|> unwrapNull obj
    <|> strMapObject obj

  arrTpls
    ∷ Array JS.Json
    → E.Either String (EJsonF JS.Json)
  arrTpls arr = do
    map Map $ map EJsonMap $ TR.traverse decodeJson arr

  strMapObject
    ∷ SM.StrMap JS.Json
    → E.Either String (EJsonF JS.Json)
  strMapObject =
    pure
    <<< Map
    <<< EJsonMap
    <<< A.fromFoldable
    <<< map (lmap encodeJson)
    <<< SM.toList

  unwrapBranch
    ∷ ∀ t
    . (TR.Traversable t, DecodeJson (t JS.Json))
    ⇒ String
    → (t JS.Json → E.Either String (EJsonF JS.Json))
    → JS.JObject
    → E.Either String (EJsonF JS.Json)
  unwrapBranch key trCodec obj =
    getOnlyKey key obj
      >>= decodeJson
      >>= trCodec

  unwrapNull
    ∷ JS.JObject
    → E.Either String (EJsonF JS.Json)
  unwrapNull =
    getOnlyKey "$na" >=>
      JS.foldJsonNull
        (E.Left "Expected null")
        (\_ → pure Null)

  unwrapLeaf
    ∷ ∀ b
    . String
    → (JS.Json → E.Either String b)
    → (b → EJsonF JS.Json)
    → JS.JObject
    → E.Either String (EJsonF JS.Json)
  unwrapLeaf key decode codec =
    getOnlyKey key
      >=> decode
      >>> map codec

  getOnlyKey
    ∷ String
    → JS.JObject
    → E.Either String JS.Json
  getOnlyKey key obj = case SM.keys obj of
    [_] →
      obj .? key
    keys →
      E.Left $ "Expected '" <> key <> "' to be the only key, but found: " <> show keys

decodeTimestamp ∷ JS.Json → E.Either String DT.DateTime
decodeTimestamp = decodeJson >=> \val →
  lmap show $ P.runParser val parseTimestamp

decodeDate ∷ JS.Json → E.Either String DT.Date
decodeDate = decodeJson >=> \val →
  lmap show $ P.runParser val parseDate

decodeTime ∷ JS.Json → E.Either String DT.Time
decodeTime = decodeJson >=> \val →
  lmap show $ P.runParser val parseTime
