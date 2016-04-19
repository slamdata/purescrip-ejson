module Data.Json.Extended.Signature.Json where

import Prelude

import Control.Alt ((<|>))
import Control.Bind ((>=>))

import Data.Argonaut.Core as JS
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Combinators ((.?))

import Data.Array as A
import Data.Either as E
import Data.HugeNum as HN
import Data.Int as Int
import Data.List as L
import Data.Maybe as M
import Data.StrMap as SM
import Data.Traversable as TR
import Data.Tuple as T

import Data.Json.Extended.Signature.Core (EJsonF(..))


encodeJsonEJsonF
  ∷ ∀ a
  . (a → JS.Json)
  → (a → M.Maybe String)
  → EJsonF a
  → JS.Json
encodeJsonEJsonF rec asKey x =
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
          T.Tuple
            <$> asKey k
            <*> pure (rec v)

        asStrMap
          ∷ Array (T.Tuple a a)
          → SM.StrMap JS.Json
        asStrMap =
          SM.fromList
            <<< L.toList
            <<< A.mapMaybe tuple

decodeJsonEJsonF
  ∷ ∀ a
  . (JS.Json → E.Either String a)
  → (String → a)
  → JS.Json
  → E.Either String (EJsonF a)
decodeJsonEJsonF rec makeKey =
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
        <<< map (\(T.Tuple k v) → T.Tuple (makeKey k) v)
        <<< SM.toList

