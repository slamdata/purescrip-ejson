module Data.Json.Extended
  ( module Sig

  , EJson(..)
  , getEJson
  , roll
  , unroll

  , null
  , boolean
  , integer
  , decimal
  , string
  , timestamp
  , date
  , time
  , interval
  , objectId
  , orderedSet
  , object
  , object'
  , array

  , renderEJson

  , arbitraryEJsonOfSize
  , arbitraryJsonEncodableEJsonOfSize
  ) where

import Prelude

import Data.Eq1 (eq1)
import Data.Ord1 (compare1)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)

import Data.Functor.Mu as Mu
import Data.HugeNum as HN
import Data.List as L
import Data.Map as Map
import Data.Maybe as M
import Data.StrMap as SM
import Data.Tuple as T

import Test.StrongCheck as SC
import Test.StrongCheck.Gen as Gen

import Data.Json.Extended.Signature as Sig

newtype EJson = EJson (Mu.Mu Sig.EJsonF)

getEJson
  ∷ EJson
  → Mu.Mu Sig.EJsonF
getEJson (EJson x) =
  x

roll
  ∷ Sig.EJsonF EJson
  → EJson
roll =
  EJson
    <<< Mu.roll
    <<< map getEJson

unroll
  ∷ EJson
  → Sig.EJsonF EJson
unroll =
  getEJson
    >>> Mu.unroll
    >>> map EJson

instance eqEJson ∷ Eq EJson where
  eq (EJson a) (EJson b) =
    eq1 (Mu.unroll a) (Mu.unroll b)

instance ordEJson ∷ Ord EJson where
  compare (EJson a) (EJson b) =
    compare1 (Mu.unroll a) (Mu.unroll b)

instance showEJson ∷ Show EJson where
  show = renderEJson

instance decodeJsonEJson ∷ DecodeJson EJson where
  decodeJson json =
    roll <$>
      Sig.decodeJsonEJsonF
        decodeJson
        (Sig.String >>> roll)
        json

-- | This is a _lossy_ encoding of EJSON to JSON; JSON only supports objects with strings
-- as keys.
instance encodeJsonEJson ∷ EncodeJson EJson where
  encodeJson (EJson x) =
    Sig.encodeJsonEJsonF
      encodeJson
      asKey
      (EJson <$> Mu.unroll x)

    where
      asKey
        ∷ EJson
        → M.Maybe String
      asKey (EJson x) =
        case Mu.unroll x of
          Sig.String k → pure k
          _ → M.Nothing


arbitraryEJsonOfSize
  ∷ Gen.Size
  → Gen.Gen EJson
arbitraryEJsonOfSize size =
  roll <$>
    case size of
      0 → Sig.arbitraryBaseEJsonF
      n → Sig.arbitraryEJsonF $ arbitraryEJsonOfSize (n - 1)

-- | Generate only JSON-encodable objects
arbitraryJsonEncodableEJsonOfSize
  ∷ Gen.Size
  → Gen.Gen EJson
arbitraryJsonEncodableEJsonOfSize size =
  roll <$>
    case size of
      0 → Sig.arbitraryBaseEJsonF
      n → Sig.arbitraryEJsonFWithKeyGen keyGen $ arbitraryJsonEncodableEJsonOfSize (n - 1)
  where
    keyGen =
      roll <<< Sig.String <$>
        SC.arbitrary

renderEJson
  ∷ EJson
  → String
renderEJson (EJson x) =
  Sig.renderEJsonF
    renderEJson
    (EJson <$> Mu.unroll x)

null ∷ EJson
null = roll Sig.Null

boolean ∷ Boolean → EJson
boolean = roll <<< Sig.Boolean

integer ∷ Int → EJson
integer = roll <<< Sig.Integer

decimal ∷ HN.HugeNum → EJson
decimal = roll <<< Sig.Decimal

string ∷ String → EJson
string = roll <<< Sig.String

timestamp ∷ String → EJson
timestamp = roll <<< Sig.Timestamp

date ∷ String → EJson
date = roll <<< Sig.Date

time ∷ String → EJson
time = roll <<< Sig.Time

interval ∷ String → EJson
interval = roll <<< Sig.Interval

objectId ∷ String → EJson
objectId = roll <<< Sig.ObjectId

orderedSet ∷ Array EJson → EJson
orderedSet = roll <<< Sig.OrderedSet

array ∷ Array EJson → EJson
array = roll <<< Sig.Array

object ∷ Map.Map EJson EJson → EJson
object = roll <<< Sig.Object <<< L.fromList <<< Map.toList

object' ∷ SM.StrMap EJson → EJson
object' = roll <<< Sig.Object <<< map go <<< L.fromList <<< SM.toList
  where
    go (T.Tuple a b) = T.Tuple (string a) b
