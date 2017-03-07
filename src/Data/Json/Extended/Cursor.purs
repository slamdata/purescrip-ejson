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

-- | A cursor to a location in an EJson value.
-- |
-- | The functions operating on cursor are "depth first", that is to say:
-- | ``` purescript
-- | atKey (EJ.string "foo") $ atIndex 0 $ atKey (EJ.string "bar") all
-- | ```
-- | Is the path:
-- | ```
-- | <value>.bar[0].foo
-- | ```
type Cursor = Mu CursorF

all ∷ Cursor
all = roll All

atKey ∷ EJ.EJson → Cursor → Cursor
atKey k = roll <<< AtKey k

atIndex ∷ Int → Cursor → Cursor
atIndex i = roll <<< AtIndex i

-- | The possible steps in a cursor.
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

-- | Peels off one layer of a cursor, if possible. The resulting tuple contains
-- | the current step (made relative), and the remainder of the cursor.
-- |
-- | ``` purescript
-- | peel (atKey (EJ.string "foo") $ atIndex 0 all) == Just (Tuple (atKey (EJ.string "foo") all) (atIndex 0 all))
-- | peel (atIndex 0 all) == Just (Tuple (atIndex 0 all) all)
-- | peel all == Nothing
-- | ```
peel ∷ Cursor → Maybe (Tuple Cursor Cursor)
peel c = case unroll c of
  All → Nothing
  AtKey k rest → Just $ Tuple (atKey k all) rest
  AtIndex i rest → Just $ Tuple (atIndex i all) rest

-- | Takes a cursor and attempts to read from an EJson value, producing the
-- | value the cursor points to, if it exists.
get ∷ Cursor → EJson → Maybe EJson
get = cata go
  where
  go :: Algebra CursorF (EJson -> Maybe EJson)
  go = case _ of
    All → Just
    AtKey k prior → getKey k <=< prior
    AtIndex i prior → getIndex i <=< prior

-- | Takes a cursor and attempts to set an EJson value within a larger EJson
-- | value if the value the cursor points at exists.
set ∷ Cursor → EJson → EJson → EJson
set cur x v = case lmap unroll <$> peel cur of
  Nothing → x
  Just (Tuple All _) → x
  Just (Tuple (AtKey k _) path) → maybe v (setKey k x) $ get path v
  Just (Tuple (AtIndex i _) path) → maybe v (setIndex i x) $ get path v

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
