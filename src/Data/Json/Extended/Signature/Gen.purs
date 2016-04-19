module Data.Json.Extended.Signature.Gen
  ( arbitraryBaseEJsonF
  , arbitraryEJsonF
  ) where

import Prelude

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

arbitraryEJsonF
  ∷ ∀ a
  . Gen.Gen a
  → Gen.Gen (EJsonF a)
arbitraryEJsonF rec =
  Gen.oneOf (pure Null)
    [ arbitraryBaseEJsonF
    , OrderedSet <$> Gen.arrayOf rec
    , Array <$> Gen.arrayOf rec
    , Object <$> Gen.arrayOf arbitraryTuple
    ]

  where
    arbitraryTuple ∷ Gen.Gen (T.Tuple a a)
    arbitraryTuple =
      T.Tuple
        <$> rec
        <*> rec

arbitraryDecimal ∷ Gen.Gen HN.HugeNum
arbitraryDecimal =
  HN.fromNumber
    <<< Data.Int.toNumber
    <$> SC.arbitrary
