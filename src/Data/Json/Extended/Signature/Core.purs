module Data.Json.Extended.Signature.Core
  ( EJsonF(..)
  , getType
  ) where

import Prelude

import Data.Bifunctor as BF
import Data.Eq (class Eq1, eq1)
import Data.Foldable as F
import Data.Traversable as T
import Data.HugeNum as HN
import Data.Int as Int
import Data.Json.Extended.Type as T
import Data.List as L
import Data.Map as Map
import Data.Monoid (mempty)
import Data.Ord (class Ord1)
import Data.Tuple (Tuple(..))

-- | The signature endofunctor for the EJson theory.
data EJsonF a
  = Null
  | String String
  | Boolean Boolean
  | Integer Int
  | Decimal HN.HugeNum
  | Timestamp String
  | Date String
  | Time String
  | Interval String
  | ObjectId String
  | Array (Array a)
  | Map (Array (Tuple a a))

instance functorEJsonF ∷ Functor EJsonF where
  map f x =
    case x of
      Null → Null
      String str → String str
      Boolean b → Boolean b
      Integer i → Integer i
      Decimal a → Decimal a
      Timestamp ts → Timestamp ts
      Date d → Date d
      Time t → Time t
      Interval i → Interval i
      ObjectId oid → ObjectId oid
      Array xs → Array $ f <$> xs
      Map xs → Map $ BF.bimap f f <$> xs

instance foldableEJsonF ∷ F.Foldable EJsonF where
  foldMap f = case _ of
    Array xs → F.foldMap f xs
    Map xs → F.foldMap (\(Tuple k v) → f k <> f v) xs
    _ → mempty
  foldl f a = case _ of
    Array xs → F.foldl f a xs
    Map xs → F.foldl (\acc (Tuple k v) → f (f acc k) v) a xs
    _ → a
  foldr f a = case _ of
    Array xs → F.foldr f a xs
    Map xs → F.foldr (\(Tuple k v) acc → f k $ f v acc) a xs
    _ → a

instance traversableEJsonF ∷ T.Traversable EJsonF where
  traverse f = case _ of
    Array xs → map Array $ T.traverse f xs
    Map xs → map Map $ T.traverse (\(Tuple k v) → Tuple <$> f k <*> f v) xs
    Null → pure Null
    String str → pure $ String str
    Boolean b → pure $ Boolean b
    Integer i → pure $ Integer i
    Decimal a → pure $ Decimal a
    Timestamp ts → pure $ Timestamp ts
    Date d → pure $ Date d
    Time t → pure $ Time t
    Interval i → pure $ Interval i
    ObjectId oid → pure $ ObjectId oid
  sequence = T.sequenceDefault



instance eq1EJsonF ∷ Eq1 EJsonF where
  eq1 Null Null = true
  eq1 (Boolean b1) (Boolean b2) = b1 == b2
  eq1 (Integer i) (Integer j) = i == j
  eq1 (Decimal a) (Decimal b) = a == b
  eq1 (Integer i) (Decimal b) = intToHugeNum i == b
  eq1 (Decimal a) (Integer j) = a == intToHugeNum j
  eq1 (String a) (String b) = a == b
  eq1 (Timestamp a) (Timestamp b) = a == b
  eq1 (Date a) (Date b) = a == b
  eq1 (Time a) (Time b) = a == b
  eq1 (Interval a) (Interval b) = a == b
  eq1 (ObjectId a) (ObjectId b) = a == b
  eq1 (Array xs) (Array ys) = xs == ys
  eq1 (Map xs) (Map ys) =
    let
      xs' = L.fromFoldable xs
      ys' = L.fromFoldable ys
    in
      isSubobject xs' ys'
        && isSubobject ys' xs'
  eq1 _ _ = false

instance eqEJsonF ∷ Eq a ⇒ Eq (EJsonF a) where
  eq = eq1

-- | Very badly performing, but we don't have access to Ord here,
-- | so the performant version is not implementable.
isSubobject
  ∷ ∀ a b
  . (Eq a, Eq b)
  ⇒ L.List (Tuple a b)
  → L.List (Tuple a b)
  → Boolean
isSubobject xs ys =
  F.foldl
    (\acc x → acc && F.elem x ys)
    true
    xs

intToHugeNum
  ∷ Int
  → HN.HugeNum
intToHugeNum =
  HN.fromNumber
    <<< Int.toNumber

derive instance ordEJsonF ∷ Ord a ⇒ Ord (EJsonF a)
instance ord1EJsonF ∷ Ord1 EJsonF where
  compare1 = compare

getType ∷ ∀ a. EJsonF a → T.EJsonType
getType = case _ of
  Null → T.Null
  String _ → T.String
  Boolean _ → T.Boolean
  Integer _ → T.Integer
  Decimal _ → T.Decimal
  Timestamp _ → T.Timestamp
  Date _ → T.Date
  Time _ → T.Time
  Interval _ → T.Interval
  ObjectId _ → T.ObjectId
  Array _ → T.Array
  Map _ → T.Map
