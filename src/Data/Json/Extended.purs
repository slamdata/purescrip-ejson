module Data.Json.Extended
  ( module Sig

  , EJson(..)
  , getEJson
  , roll
  , unroll

  , renderEJson

  , arbitraryEJsonOfSize
  , arbitraryJsonEncodableEJsonOfSize
  ) where

import Prelude

import Data.Eq1 (eq1)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)

import Data.Functor.Mu as Mu
import Data.Maybe as M

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

instance showEJson ∷ Show EJson where
  show = renderEJson

instance decodeJsonEJson ∷ DecodeJson EJson where
  decodeJson json =
    map roll $
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

