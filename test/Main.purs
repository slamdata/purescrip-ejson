module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either as E
import Data.Json.Extended (EJson, arbitraryJsonEncodableEJsonOfSize)

import Test.StrongCheck as SC

type TestEffects =
  ( err ∷ EXCEPTION
  , random ∷ RANDOM
  , console ∷ CONSOLE
  )

newtype ArbEJson = ArbEJson EJson

instance arbitraryArbEJson ∷ SC.Arbitrary ArbEJson where
  arbitrary = ArbEJson <$> arbitraryJsonEncodableEJsonOfSize 2

main :: Eff TestEffects Unit
main = do
  SC.quickCheck \(ArbEJson x) →
    case decodeJson (encodeJson x) of
      E.Right y → x == y SC.<?> "Mismatch:\n" <> show x <> "\n" <> show y
      E.Left err → SC.Failed $ "Parse error: " <> err

hole ∷ ∀ a. a
hole = Unsafe.Coerce.unsafeCoerce "a"
