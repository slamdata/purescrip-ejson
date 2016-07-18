module Data.Json.Extended.Signature.Core
  ( EJsonF(..)
  ) where

import Prelude

import Data.Bifunctor as BF
import Data.Eq1 (class Eq1)
import Data.Foldable as F
import Data.HugeNum as HN
import Data.Int as Int
import Data.List as L
import Data.Map as Map
import Data.Ord1 (class Ord1)
import Data.Tuple as T

-- | The signature endofunctor for the EJson theory.
data EJsonF a
  = Null
  | String String
  | Boolean Boolean
  | Integer Int
  | Decimal HN.HugeNum
  | String String
  | Timestamp String
  | Date String
  | Time String
  | Interval String
  | ObjectId String
  | Array (Array a)
  | Object (Array (T.Tuple a a))

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
      Object xs → Object $ BF.bimap f f <$> xs

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
  eq1 (Object xs) (Object ys) =
    let
      xs' = L.fromFoldable xs
      ys' = L.fromFoldable ys
    in
      isSubobject xs' ys'
        && isSubobject ys' xs'
  eq1 _ _ = false

-- | Very badly performing, but we don't have access to Ord here,
-- | so the performant version is not implementable.
isSubobject
  ∷ ∀ a b
  . (Eq a, Eq b)
  ⇒ L.List (T.Tuple a b)
  → L.List (T.Tuple a b)
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

instance ord1EJsonF ∷ Ord1 EJsonF where
  compare1 Null Null = EQ
  compare1 _ Null = GT
  compare1 Null _ = LT

  compare1 (Boolean b1) (Boolean b2) = compare b1 b2
  compare1 _ (Boolean _) = GT
  compare1 (Boolean _) _ = LT

  compare1 (Integer i) (Integer j) = compare i j
  compare1 (Integer i) (Decimal b) = compare (intToHugeNum i) b
  compare1 (Decimal a) (Integer j) = compare a (intToHugeNum j)
  compare1 _ (Integer _) = GT
  compare1 (Integer _) _ = LT

  compare1 (Decimal a) (Decimal b) = compare a b
  compare1 _ (Decimal _) = GT
  compare1 (Decimal _) _ = LT

  compare1 (String a) (String b) = compare a b
  compare1 _ (String _) = GT
  compare1 (String _) _ = LT

  compare1 (Timestamp a) (Timestamp b) = compare a b
  compare1 _ (Timestamp _) = GT
  compare1 (Timestamp _) _ = LT

  compare1 (Date a) (Date b) = compare a b
  compare1 _ (Date _) = GT
  compare1 (Date _) _ = LT

  compare1 (Time a) (Time b) = compare a b
  compare1 _ (Time _) = GT
  compare1 (Time _) _ = LT

  compare1 (Interval a) (Interval b) = compare a b
  compare1 _ (Interval _) = GT
  compare1 (Interval _) _ = LT

  compare1 (ObjectId a) (ObjectId b) = compare a b
  compare1 _ (ObjectId _) = GT
  compare1 (ObjectId _) _ = LT

  compare1 (Array a) (Array b) = compare a b
  compare1 _ (Array _) = GT
  compare1 (Array _) _ = LT

  compare1 (Object a) (Object b) = compare (pairsToObject a) (pairsToObject b)

pairsToObject
  ∷ ∀ a b
  . (Ord a)
  ⇒ Array (T.Tuple a b)
  → Map.Map a b
pairsToObject = Map.fromFoldable
