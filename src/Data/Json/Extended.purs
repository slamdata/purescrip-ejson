module Data.Json.Extended
  ( module Sig

  , EJson(..)
  , getEJson

  , class EJsonView
  , intoEJson
  , outEJson

  , renderEJson

  , arbitraryEJsonOfSize
  , arbitraryJsonEncodableEJsonOfSize
  ) where

import Prelude

import Data.Eq1 (eq1)
import Data.Argonaut.Core as JS
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Decode (class DecodeJson)

import Data.Either as E
import Data.Functor.Mu as Mu
import Data.Maybe as M

import Test.StrongCheck as SC
import Test.StrongCheck.Gen as Gen

import Data.Json.Extended.Signature as Sig

class EJsonView a where
  intoEJson ∷ Sig.EJsonF a → a
  outEJson ∷ a → E.Either a (Sig.EJsonF a)

newtype EJson = EJson (Mu.Mu Sig.EJsonF)

getEJson ∷ EJson → Mu.Mu Sig.EJsonF
getEJson (EJson x) = x

instance ejsonViewEJson ∷ EJsonView EJson where
  intoEJson = EJson <<< Mu.roll <<< map getEJson
  outEJson = getEJson >>> Mu.unroll >>> map EJson >>> E.Right

instance eqEJson ∷ Eq EJson where
  eq (EJson a) (EJson b) =
    eq1 (Mu.unroll a) (Mu.unroll b)

instance showEJson ∷ Show EJson where
  show = renderEJson

instance _decodeJsonEJson ∷ DecodeJson EJson where
  decodeJson = decodeJsonEJson

instance _encodeJsonEJson ∷ EncodeJson EJson where
  encodeJson = encodeJsonEJson

-- | This is a _lossy_ encoding of EJSON to JSON; JSON only supports objects with strings
-- as keys.
encodeJsonEJson
  ∷ EJson
  → JS.Json
encodeJsonEJson (EJson x) =
  Sig.encodeJsonEJsonF
    encodeJsonEJson
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

decodeJsonEJson
  ∷ ∀ a
  . (EJsonView a)
  ⇒ JS.Json
  → E.Either String a
decodeJsonEJson x =
  map intoEJson $
    Sig.decodeJsonEJsonF
      decodeJsonEJson
      (Sig.String >>> intoEJson)
      x

arbitraryEJsonOfSize
  ∷ ∀ a
  . (Eq a, EJsonView a)
  ⇒ Gen.Size
  → Gen.Gen a
arbitraryEJsonOfSize size =
  intoEJson <$>
    case size of
      0 → Sig.arbitraryBaseEJsonF
      n → Sig.arbitraryEJsonF $ arbitraryEJsonOfSize (n - 1)

-- | Generate only JSON-encodable objects
arbitraryJsonEncodableEJsonOfSize
  ∷ ∀ a
  . (Eq a, EJsonView a)
  ⇒ Gen.Size
  → Gen.Gen a
arbitraryJsonEncodableEJsonOfSize size =
  intoEJson <$>
    case size of
      0 → Sig.arbitraryBaseEJsonF
      n → Sig.arbitraryEJsonFWithKeyGen keyGen $ arbitraryJsonEncodableEJsonOfSize (n - 1)

  where
    keyGen =
      intoEJson <<< Sig.String <$>
        SC.arbitrary

renderEJson
  ∷ EJson
  → String
renderEJson (EJson x) =
  Sig.renderEJsonF
    renderEJson
    (EJson <$> Mu.unroll x)

