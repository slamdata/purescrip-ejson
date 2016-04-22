module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either as E
import Data.Json.Extended (EJson, arbitraryJsonEncodableEJsonOfSize, arbitraryEJsonOfSize, renderEJson, parseEJson)

import Text.Parsing.Parser as P

import Test.StrongCheck as SC

type TestEffects =
  ( err ∷ EXCEPTION
  , random ∷ RANDOM
  , console ∷ CONSOLE
  )

newtype ArbJsonEncodableEJson = ArbJsonEncodableEJson EJson
newtype ArbEJson = ArbEJson EJson

instance arbitraryArbJsonEncodableEJson ∷ SC.Arbitrary ArbJsonEncodableEJson where
  arbitrary = ArbJsonEncodableEJson <$> arbitraryJsonEncodableEJsonOfSize 2

instance arbitraryArbEJson ∷ SC.Arbitrary ArbEJson where
  arbitrary = ArbEJson <$> arbitraryEJsonOfSize 2

testJsonSerialization ∷ Eff TestEffects Unit
testJsonSerialization =
  SC.quickCheck \(ArbJsonEncodableEJson x) →
    case decodeJson (encodeJson x) of
      E.Right y → x == y SC.<?> "Mismatch:\n" <> show x <> "\n" <> show y
      E.Left err → SC.Failed $ "Parse error: " <> err

testRenderParse ∷ Eff TestEffects Unit
testRenderParse =
  SC.quickCheck \(ArbEJson x) →
    case P.runParser (renderEJson x) parseEJson of
      E.Right y → x == y SC.<?> "Mismatch:\n" <> show x <> "\n" <> show y
      E.Left err → SC.Failed $ "Parse error: " <> show err <> " when parsing:\n\n " <> renderEJson x <> "\n\n"

main :: Eff TestEffects Unit
main = do
  testJsonSerialization
  testRenderParse
