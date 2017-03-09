module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either as E
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Data.Json.Extended (EJson, arbitraryJsonEncodableEJsonOfSize, arbitraryEJsonOfSize, renderEJson, parseEJson)
import Data.Json.Extended as EJ
import Data.Json.Extended.Cursor as EJC

import Text.Parsing.Parser as P

import Test.StrongCheck as SC
import Test.StrongCheck.Arbitrary as SCA

type TestEffects =
  ( err ∷ EXCEPTION
  , random ∷ RANDOM
  , console ∷ CONSOLE
  )

newtype ArbJsonEncodableEJson = ArbJsonEncodableEJson EJson
newtype ArbEJson = ArbEJson EJson

instance arbitraryArbJsonEncodableEJson ∷ SCA.Arbitrary ArbJsonEncodableEJson where
  arbitrary = ArbJsonEncodableEJson <$> arbitraryJsonEncodableEJsonOfSize 2

instance arbitraryArbEJson ∷ SCA.Arbitrary ArbEJson where
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

testCursorExamples ∷ Eff TestEffects Unit
testCursorExamples = do
  assertEq
    (EJC.peel (EJC.atKey (EJ.string "foo") $ EJC.atIndex 0 EJC.all))
    (Just (Tuple (EJC.atKey (EJ.string "foo") EJC.all) (EJC.atIndex 0 EJC.all)))
  assertEq
    (EJC.peel (EJC.atIndex 0 EJC.all))
    (Just (Tuple (EJC.atIndex 0 EJC.all) EJC.all))
  assertEq
    (EJC.peel EJC.all)
    Nothing
  assertEq
    (EJC.getKey (EJ.string "foo") (EJ.map' $ EJ.string <$> SM.fromFoldable [Tuple "foo" "bar"]))
    (Just (EJ.string "bar"))
  assertEq
    (EJC.getKey (EJ.string "foo") (EJ.map' $ EJ.string <$> SM.fromFoldable [Tuple "key" "value"]))
    Nothing
  assertEq
    (EJC.getKey (EJ.string "foo") (EJ.boolean false))
    Nothing
  assertEq
    (EJC.getIndex 0 (EJ.array $ EJ.string <$> ["foo"]))
    (Just (EJ.string "foo"))
  assertEq
    (EJC.getIndex 1 (EJ.array $ EJ.string <$> ["foo"]))
    Nothing
  assertEq
    (EJC.getIndex 0 (EJ.boolean false))
    Nothing
  let map = EJ.map' $ EJ.string <$> SM.fromFoldable [Tuple "foo" "bar"]
  assertEq
    (EJC.setKey (EJ.string "foo") (EJ.boolean true) map)
    (EJ.map' (SM.fromFoldable [Tuple "foo" (EJ.boolean true)]))
  assertEq
    (EJC.setKey (EJ.string "bar") (EJ.boolean true) map)
    map
  assertEq
    (EJC.setKey (EJ.string "foo") (EJ.boolean true) (EJ.string "not-a-map"))
    (EJ.string "not-a-map")
  let array = EJ.array $ EJ.string <$> ["foo"]
  assertEq
    (EJC.setIndex 0 (EJ.boolean true) array)
    (EJ.array [EJ.boolean true])
  assertEq
    (EJC.setIndex 1 (EJ.boolean true) array)
    array
  assertEq
    (EJC.setIndex 0 (EJ.boolean true) (EJ.string "not-an-array"))
    (EJ.string "not-an-array")
  where
  assertEq ∷ ∀ a. (Show a, Eq a) ⇒ a → a → Eff TestEffects Unit
  assertEq x y = SC.assert $ SC.assertEq x y

main :: Eff TestEffects Unit
main = do
  testJsonSerialization
  testRenderParse
  testCursorExamples
