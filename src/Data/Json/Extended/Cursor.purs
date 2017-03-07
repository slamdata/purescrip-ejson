module Data.Json.Extended.Cursor where

import Prelude

import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Eq (class Eq1)
import Data.Functor.Mu (Mu, roll, unroll)
import Data.Json.Extended (EJson)
import Data.Json.Extended as EJ
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (class Ord1)
import Data.Tuple (Tuple(..), lookup)

import Matryoshka (Algebra, cata)

data CursorF a
  = All
  | AtKey EJson a
  | AtIndex Int a

derive instance functorCursorF ∷ Functor CursorF
derive instance eqCursor ∷ Eq a ⇒ Eq (CursorF a)
derive instance ordCursor ∷ Ord a ⇒ Ord (CursorF a)

instance eq1CursorF ∷ Eq1 CursorF where
  eq1 = eq

instance ord1CursorF ∷ Ord1 CursorF where
  compare1 = compare

instance showCursorF ∷ Show a => Show (CursorF a) where
  show = case _ of
    All → "All"
    AtKey k a → "(AtKey " <> show k <> " " <> show a <> ")"
    AtIndex i a → "(AtIndex " <> show i <> " " <> show a <> ")"

type Cursor = Mu CursorF

all ∷ Cursor
all = roll All

atKey ∷ EJ.EJson → Cursor → Cursor
atKey k = roll <<< AtKey k

atIndex ∷ Int → Cursor → Cursor
atIndex i = roll <<< AtIndex i

peel ∷ Cursor → Maybe (Tuple Cursor Cursor)
peel c = case unroll c of
  All → Nothing
  AtKey k rest → Just $ Tuple (atKey k all) rest
  AtIndex i rest → Just $ Tuple (atIndex i all) rest

get ∷ Cursor → EJson → Maybe EJson
get = cata go
  where
  go :: Algebra CursorF (EJson -> Maybe EJson)
  go = case _ of
    All → Just
    AtKey k prior → getKey k <=< prior
    AtIndex i prior → getIndex i <=< prior

set ∷ Cursor → EJson → EJson → Maybe EJson
set cur x v = case lmap unroll <$> peel cur of
  Nothing → Just x
  Just (Tuple All _) → Just x
  Just (Tuple (AtKey k _) path) → setKey k x <$> get path v
  Just (Tuple (AtIndex i _) path) → setIndex i x <$> get path v

getKey ∷ EJ.EJson → EJ.EJson → Maybe EJ.EJson
getKey k v = case EJ.head v of
  EJ.Map fields → EJ.EJson <$> lookup (EJ.getEJson k) fields
  _ → Nothing

setKey ∷ EJ.EJson → EJ.EJson → EJ.EJson → EJ.EJson
setKey k (EJ.EJson x) v = case EJ.head v of
  EJ.Map fields →
    EJ.EJson <<< roll <<< EJ.Map $ map
      (\(kv@(Tuple k v)) → if k == k then Tuple k x else kv) fields
  _ → v

getIndex ∷ Int → EJ.EJson → Maybe EJ.EJson
getIndex i v = case EJ.head v of
  EJ.Array items → EJ.EJson <$> A.index items i
  _ → Nothing

setIndex ∷ Int → EJ.EJson → EJ.EJson → EJ.EJson
setIndex i (EJ.EJson x) v = case EJ.head v of
  EJ.Array items →
    maybe v (EJ.EJson <<< roll <<< EJ.Array) $ A.updateAt i x items
  _ → v
