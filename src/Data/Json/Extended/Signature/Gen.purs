module Data.Json.Extended.Signature.Gen
  ( arbitraryBaseEJsonF
  , arbitraryEJsonF
  , arbitraryEJsonFWithKeyGen
  ) where

import Prelude

import Data.Array as A
import Data.Json.Extended.Signature.Core (EJsonF(..))
import Data.Tuple as T
import Data.HugeNum as HN

import Test.StrongCheck as SC
import Test.StrongCheck.Gen as Gen

arbitraryBaseEJsonF ∷ ∀ a. Gen.Gen (EJsonF a)
arbitraryBaseEJsonF =
  Gen.oneOf (pure Null)
    [ Boolean <$> SC.arbitrary
    , Integer <$> SC.arbitrary
    , Decimal <$> arbitraryDecimal
    , String <$> SC.arbitrary
    , Timestamp <$> SC.arbitrary
    , Date <$> SC.arbitrary
    , Time <$> SC.arbitrary
    , Interval <$> SC.arbitrary
    , ObjectId <$> SC.arbitrary
    , pure Null
    ]

arbitraryEJsonFWithKeyGen
  ∷ ∀ a
  . (Eq a)
  ⇒ Gen.Gen a
  → Gen.Gen a
  → Gen.Gen (EJsonF a)
arbitraryEJsonFWithKeyGen keyGen rec =
  Gen.oneOf (pure Null)
    [ arbitraryBaseEJsonF
    , Array <$> Gen.arrayOf rec
    , Object <$> do
        keys ← distinctArrayOf keyGen
        vals ← Gen.vectorOf (A.length keys) rec
        pure $ A.zip keys vals
    ]

  where
    arbitraryTuple ∷ Gen.Gen (T.Tuple a a)
    arbitraryTuple =
      T.Tuple
        <$> keyGen
        <*> rec

arbitraryEJsonF
  ∷ ∀ a
  . (Eq a)
  ⇒ Gen.Gen a
  → Gen.Gen (EJsonF a)
arbitraryEJsonF rec =
  arbitraryEJsonFWithKeyGen rec rec

distinctArrayOf
  ∷ ∀ a
  . (Eq a)
  ⇒ Gen.Gen a
  → Gen.Gen (Array a)
distinctArrayOf =
  map A.nub
    <<< Gen.arrayOf

arbitraryDecimal ∷ Gen.Gen HN.HugeNum
arbitraryDecimal =
  HN.fromNumber
    <$> SC.arbitrary
