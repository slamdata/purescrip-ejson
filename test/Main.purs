module Test.Main where

import Prelude

import Data.Either as E
import Data.Json.Extended (EJson, arbitraryEJsonOfSize, renderEJson, parseEJson)
import Data.Json.Extended as EJ
import Data.Json.Extended.Cursor as EJC
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.QuickCheck ((<?>))
import Test.QuickCheck as QC
import Test.QuickCheck.Arbitrary as QCA
import Text.Parsing.Parser as P

newtype ArbEJson = ArbEJson EJson

instance arbitraryArbEJson ∷ QCA.Arbitrary ArbEJson where
  arbitrary = map ArbEJson $ arbitraryEJsonOfSize 3

testRenderParse ∷ Effect Unit
testRenderParse =
  QC.quickCheck' 1000 \(ArbEJson x) → case P.runParser (renderEJson x) parseEJson of
    E.Right y →
      x == y <?> "Mismatch:\n" <> renderEJson x <> "\n" <> renderEJson y
    E.Left err →
      QC.Failed $ "Parse error: " <> show err <> " when parsing:\n\n " <> renderEJson x <> "\n\n"

testCursorExamples ∷ Effect Unit
testCursorExamples = do
  assertMbTplEq
    (EJC.peel (EJC.atKey (EJ.string "foo") $ EJC.atIndex 0 EJC.all))
    (Just (Tuple (EJC.atKey (EJ.string "foo") EJC.all) (EJC.atIndex 0 EJC.all)))
  assertMbTplEq
    (EJC.peel (EJC.atIndex 0 EJC.all))
    (Just (Tuple (EJC.atIndex 0 EJC.all) EJC.all))
  assertMbTplEq
    (EJC.peel EJC.all)
    Nothing
  assertMbEq
    (EJC.getKey (EJ.string "foo") (EJ.map $ M.fromFoldable [Tuple (EJ.string "foo") (EJ.string "bar")]))
    (Just (EJ.string "bar"))
  assertMbEq
    (EJC.getKey (EJ.string "foo") (EJ.map $ M.fromFoldable [Tuple (EJ.string "key") (EJ.string "value")]))
    Nothing
  assertMbEq
    (EJC.getKey (EJ.string "foo") (EJ.boolean false))
    Nothing
  assertMbEq
    (EJC.getIndex 0 (EJ.array $ EJ.string <$> ["foo"]))
    (Just (EJ.string "foo"))
  assertMbEq
    (EJC.getIndex 1 (EJ.array $ EJ.string <$> ["foo"]))
    Nothing
  assertMbEq
    (EJC.getIndex 0 (EJ.boolean false))
    Nothing
  let map = EJ.map $ M.fromFoldable [Tuple (EJ.string "foo") (EJ.string "bar")]
  assertEq
    (EJC.setKey (EJ.string "foo") (EJ.boolean true) map)
    (EJ.map (M.fromFoldable [Tuple (EJ.string "foo") (EJ.boolean true)]))
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
  assertMbTplEq
    ∷ Maybe (Tuple EJC.Cursor EJC.Cursor)
    → Maybe (Tuple EJC.Cursor EJC.Cursor)
    → Effect Unit
  assertMbTplEq x y =
    QC.quickCheck' 1 $
      QC.assertEquals
        (map (\(Tuple a b) → "Tuple " <> EJC.renderEJsonCursor a <> " " <> EJC.renderEJsonCursor b) x)
        (map (\(Tuple a b) → "Tuple " <> EJC.renderEJsonCursor a <> " " <> EJC.renderEJsonCursor b) y)

  assertMbEq ∷ Maybe EJson → Maybe EJson → Effect Unit
  assertMbEq x y = QC.quickCheck' 1 $ QC.assertEquals (map renderEJson x) (map renderEJson y)

  assertEq ∷ EJson → EJson → Effect Unit
  assertEq x y = QC.quickCheck' 1 $ x == y <?> msg
    where
    msg = renderEJson x <> " /= " <> renderEJson y

main ∷ Effect Unit
main = do
  testRenderParse
  testCursorExamples
