module Data.Json.Extended.Signature.Core
  ( EJsonF(..)
  , EJsonMap(..)
  , getType
  ) where

import Prelude

import Data.Bifunctor as BF
import Data.Eq (class Eq1)
import Data.Foldable as F
import Data.HugeNum as HN
import Data.HugeInt as HI
import Data.Json.Extended.Type as JT
import Data.List as L
import Data.Map as M
import Data.Monoid (mempty)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)
import Data.TacitString (TacitString)
import Data.Traversable as T
import Data.Tuple (Tuple(..))

-- | The signature endofunctor for the EJson theory.
data EJsonF a
  = Null
  | String String
  | Boolean Boolean
  | Integer HI.HugeInt
  | Decimal HN.HugeNum
  | Array (Array a)
  | Map (EJsonMap a)

derive instance functorEJsonF ∷ Functor EJsonF

derive instance eqEJsonF ∷ Eq a ⇒ Eq (EJsonF a)
instance eq1EJsonF ∷ Eq1 EJsonF where eq1 = eq

derive instance ordEJsonF ∷ Ord a ⇒ Ord (EJsonF a)
instance ord1EJsonF ∷ Ord1 EJsonF where compare1 = compare

instance foldableEJsonF ∷ F.Foldable EJsonF where
  foldMap f = case _ of
    Array xs → F.foldMap f xs
    Map xs → F.foldMap f xs
    _ → mempty
  foldl f a = case _ of
    Array xs → F.foldl f a xs
    Map xs → F.foldl f a xs
    _ → a
  foldr f a = case _ of
    Array xs → F.foldr f a xs
    Map xs → F.foldr f a xs
    _ → a

instance traversableEJsonF ∷ T.Traversable EJsonF where
  traverse f = case _ of
    Array xs → Array <$> T.traverse f xs
    Map xs → Map <$> T.traverse f xs
    Null → pure Null
    String str → pure $ String str
    Boolean b → pure $ Boolean b
    Integer i → pure $ Integer i
    Decimal d → pure $ Decimal d
  sequence = T.sequenceDefault

instance showEJsonF ∷ Show (EJsonF TacitString) where
  show = case _ of
    Null → "Null"
    String s → "(String " <> show s <> ")"
    Boolean b → "(Boolean " <> show b <> ")"
    Integer i → "(Integer " <> show i <> ")"
    Decimal d → "(Decimal " <> show d <> ")"
    Array xs → "(Array " <> show xs <> ")"
    Map kvs → "(Map " <> show kvs <> ")"

getType ∷ ∀ a. EJsonF a → JT.EJsonType
getType = case _ of
  Null → JT.Null
  String _ → JT.String
  Boolean _ → JT.Boolean
  Integer _ → JT.Integer
  Decimal _ → JT.Decimal
  Array _ → JT.Array
  Map _ → JT.Map

newtype EJsonMap a = EJsonMap (Array (Tuple a a))

derive instance newtypeEJsonMap ∷ Newtype (EJsonMap a) _

instance functorEJsonMap ∷ Functor EJsonMap where
  map f (EJsonMap xs) = EJsonMap (BF.bimap f f <$> xs)

instance eqEJsonMap ∷ Eq a ⇒ Eq (EJsonMap a) where
  eq (EJsonMap xs) (EJsonMap ys) =
    let
      xs' = L.fromFoldable xs
      ys' = L.fromFoldable ys
    in
      isSubobject xs' ys'
        && isSubobject ys' xs'

-- | Very badly performing, but we don't have access to Ord here,
-- | so the performant version is not implementable.
isSubobject ∷ ∀ a. Eq a ⇒ L.List (Tuple a a) → L.List (Tuple a a) → Boolean
isSubobject xs ys = F.foldl (\acc x → acc && F.elem x ys) true xs

instance ordEJsonMap ∷ Ord a ⇒ Ord (EJsonMap a) where
  compare (EJsonMap xs) (EJsonMap ys) =
    compare (M.fromFoldable xs) (M.fromFoldable ys)

instance showEJsonMap ∷ Show (EJsonMap TacitString) where
  show (EJsonMap xs) = "(EJsonMap " <> show xs <> ")"

instance foldableEJsonMap ∷ F.Foldable EJsonMap where
  foldMap f (EJsonMap xs) = F.foldMap (\(Tuple k v) → f k <> f v) xs
  foldl f a (EJsonMap xs) = F.foldl (\acc (Tuple k v) → f (f acc k) v) a xs
  foldr f a (EJsonMap xs) = F.foldr (\(Tuple k v) acc → f k $ f v acc) a xs

instance traversableEJsonMap ∷ T.Traversable EJsonMap where
  traverse f (EJsonMap xs) =
    EJsonMap <$> T.traverse (\(Tuple k v) → Tuple <$> f k <*> f v) xs
  sequence = T.sequenceDefault
