module Data.Json.Extended.Cursor where

import Prelude

import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Functor.Mu (Mu)
import Data.Json.Extended (EJson)
import Data.Json.Extended as EJ
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Ord (class Ord1)
import Data.TacitString (TacitString)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..), lookup)

import Matryoshka (Algebra, cata, project, embed)

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
all = embed All

atKey ∷ EJson → Cursor → Cursor
atKey k = embed <<< AtKey k

atIndex ∷ Int → Cursor → Cursor
atIndex i = embed <<< AtIndex i

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

instance showCursorF ∷ Show (CursorF TacitString) where
  show = case _ of
    All → "All"
    AtKey k a → "(AtKey " <> show k <> " " <> show a <> ")"
    AtIndex i a → "(AtIndex " <> show i <> " " <> show a <> ")"

instance foldableCursorF :: Foldable CursorF where
  foldr f b = case _ of
    All -> b
    AtKey _ a -> f a b
    AtIndex _ a -> f a b
  foldl f b = case _ of
    All -> b
    AtKey _ a -> f b a
    AtIndex _ a -> f b a
  foldMap f = case _ of
    All -> mempty
    AtKey _ a -> f a
    AtIndex _ a -> f a

instance traversableCursorF :: Traversable CursorF where
  traverse f = case _ of
    All -> pure All
    AtKey k a -> AtKey k <$> f a
    AtIndex i a -> AtIndex i <$> f a
  sequence = traverse id

renderEJsonCursor ∷ Cursor → String
renderEJsonCursor = show

-- | Peels off one layer of a cursor, if possible. The resulting tuple contains
-- | the current step (made relative), and the remainder of the cursor.
-- |
-- | ``` purescript
-- | peel (atKey (EJ.string "foo") $ atIndex 0 all) == Just (Tuple (atKey (EJ.string "foo") all) (atIndex 0 all))
-- | peel (atIndex 0 all) == Just (Tuple (atIndex 0 all) all)
-- | peel all == Nothing
-- | ```
peel ∷ Cursor → Maybe (Tuple Cursor Cursor)
peel c = case project c of
  All → Nothing
  AtKey k rest → Just $ Tuple (atKey k all) rest
  AtIndex i rest → Just $ Tuple (atIndex i all) rest

-- | Takes a cursor and attempts to read from an EJson value, producing the
-- | value the cursor points to, if it exists.
get ∷ Cursor → EJson → Maybe EJson
get = cata go
  where
  go ∷ Algebra CursorF (EJson → Maybe EJson)
  go = case _ of
    All → Just
    AtKey k prior → getKey k <=< prior
    AtIndex i prior → getIndex i <=< prior

-- | Takes a cursor and attempts to set an EJson value within a larger EJson
-- | value if the value the cursor points at exists.
set ∷ Cursor → EJson → EJson → EJson
set cur x v = case lmap project <$> peel cur of
  Nothing → x
  Just (Tuple All _) → x
  Just (Tuple (AtKey k _) path) → maybe v (setKey k x) $ get path v
  Just (Tuple (AtIndex i _) path) → maybe v (setIndex i x) $ get path v

-- | Attempts to lookup a key in an EJson Map, returning the associated value
-- | if the key exists and the value is a Map.
-- |
-- | ``` purescript
-- | getKey (EJ.string "foo") (EJ.map' $ EJ.string <$> SM.fromFoldable [Tuple "foo" "bar"]) == Just (EJ.string "bar")
-- | getKey (EJ.string "foo") (EJ.map' $ EJ.string <$> SM.fromFoldable [Tuple "key" "value"]) == Nothing
-- | getKey (EJ.string "foo") (EJ.boolean false) == Nothing
-- | ```
getKey ∷ EJ.EJson → EJ.EJson → Maybe EJ.EJson
getKey k v = case project v of
  EJ.Map (EJ.EJsonMap fields) → lookup k fields
  _ → Nothing

-- | For a given key, attempts to set a new value for it in an EJson Map. If the
-- | value is not a Map, or the key does not already exist, the original value
-- | is returned.
-- |
-- | ``` purescript
-- | let map = EJ.map' $ EJ.string <$> SM.fromFoldable [Tuple "foo" "bar"]
-- | setKey (EJ.string "foo") (EJ.boolean true) map == EJ.map' (SM.fromFoldable [Tuple "foo" (EJ.boolean true)])
-- | setKey (EJ.string "bar") (EJ.boolean true) map == map
-- | setKey (EJ.string "foo") (EJ.boolean true) (EJ.string "not-a-map") == EJ.string "not-a-map"
-- | ```
setKey ∷ EJ.EJson → EJ.EJson → EJ.EJson → EJ.EJson
setKey k x v = case project v of
  EJ.Map (EJ.EJsonMap fields) →
    embed <<< EJ.Map <<< EJ.EJsonMap $ map
      (\(kv@(Tuple k' v)) → if k == k' then Tuple k x else kv) fields
  _ → v

-- | Attempts to lookup an index in an EJson Array, returning the associated
-- | value if there is an item at that index, and the value is an Array.
-- |
-- | ``` purescript
-- | getIndex 0 (EJ.array $ EJ.string <$> ["foo"]) == Just (EJ.string "foo")
-- | getIndex 1 (EJ.array $ EJ.string <$> ["foo"]) == Nothing
-- | getIndex 0 (EJ.boolean false) == Nothing
-- | ```
getIndex ∷ Int → EJ.EJson → Maybe EJ.EJson
getIndex i v = case project v of
  EJ.Array items → A.index items i
  _ → Nothing

-- | For a given index, attempts to set a new value for it in an EJson Array. If
-- | the value is not a Array, or the index does not already exist, the original
-- | value is returned.
-- |
-- | ``` purescript
-- | let array = EJ.array $ EJ.string <$> ["foo"]
-- | setIndex 0 (EJ.boolean true) array == EJ.array [EJ.boolean true]
-- | setIndex 1 (EJ.boolean true) array == array
-- | setIndex 0 (EJ.boolean true) (EJ.string "not-an-array") == EJ.string "not-an-array"
-- | ```
setIndex ∷ Int → EJ.EJson → EJ.EJson → EJ.EJson
setIndex i x v = case project v of
  EJ.Array items →
    maybe v (embed <<< EJ.Array) $ A.updateAt i x items
  _ → v
