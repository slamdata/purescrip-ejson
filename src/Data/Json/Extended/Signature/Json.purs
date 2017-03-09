module Data.Json.Extended.Signature.Json where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((>=>))

import Data.Bifunctor (lmap, bimap)
import Data.Eq (class Eq1)
import Data.Ord (class Ord1)
import Data.Functor.Coproduct (Coproduct(..), coproduct, right, left)
import Data.Argonaut.Core as JS
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode (encodeJson)
import Data.Array as A
import Data.Either as E
import Data.HugeNum as HN
import Data.Int as Int
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Json.Extended.Signature.Core (EJsonF(..))
import Data.Maybe as M
import Data.StrMap as SM
import Data.Traversable as TR
import Data.Tuple as T

import Matryoshka (Algebra, CoalgebraM)

encodeJsonEJsonF ∷ Algebra EJsonF JS.Json
encodeJsonEJsonF = case _ of
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
  Array xs → encodeJson xs
  Map xs → JS.jsonSingletonObject "$obj" $ encodeJson $ asStrMap xs
  where
  tuple
    ∷ T.Tuple JS.Json JS.Json
    → M.Maybe (T.Tuple String JS.Json)
  tuple (T.Tuple k v) =
    T.Tuple  <$> JS.toString k  <*> pure v

  asStrMap
    ∷ Array (T.Tuple JS.Json JS.Json)
    → SM.StrMap JS.Json
  asStrMap =
    SM.fromFoldable
    <<< A.mapMaybe tuple


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
    <|> unwrapLeaf "$timestamp" Timestamp obj
    <|> unwrapLeaf "$date" Date obj
    <|> unwrapLeaf "$time" Time obj
    <|> unwrapLeaf "$interval" Interval obj
    <|> unwrapLeaf "$oid" ObjectId obj
    <|> unwrapNull obj
    <|> (pure $ strMapObject obj)

  strMapObject
    ∷ SM.StrMap JS.Json
    → EJsonF JS.Json
  strMapObject =
    Map
    <<< A.fromFoldable
    <<< map (lmap encodeJson)
    <<< SM.toList

  unwrapBranch
    ∷ ∀ t
    . (TR.Traversable t, DecodeJson (t JS.Json))
    ⇒ String
    → (t JS.Json → EJsonF JS.Json)
    → JS.JObject
    → E.Either String (EJsonF JS.Json)
  unwrapBranch key trCodec obj =
    getOnlyKey key obj
      >>= decodeJson
      >>> map trCodec

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
    . (DecodeJson b)
    ⇒ String
    → (b → EJsonF JS.Json)
    → JS.JObject
    → E.Either String (EJsonF JS.Json)
  unwrapLeaf key codec =
    getOnlyKey key
      >=> decodeJson
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
