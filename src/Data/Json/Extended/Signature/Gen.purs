module Data.Json.Extended.Signature.Gen
  ( arbitraryBaseEJsonF
  , arbitraryEJsonF
  , arbitraryEJsonFWithKeyGen
  ) where

import Prelude

import Data.Array as A
import Data.DateTime as DT
import Data.Enum (toEnum)
import Data.HugeNum as HN
import Data.Json.Extended.Signature.Core (EJsonF(..), EJsonMap(..))
import Data.Maybe (fromMaybe)
import Data.Tuple as T

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

arbitraryBaseEJsonF ∷ ∀ a. Gen.Gen (EJsonF a)
arbitraryBaseEJsonF =
  Gen.oneOf (pure Null)
    [ Boolean <$> SC.arbitrary
    , Integer <$> SC.arbitrary
    , Decimal <$> arbitraryDecimal
    , String <$> SC.arbitrary
    , Timestamp <$> arbitraryDateTime
    , Date <$> arbitraryDate
    , Time <$> arbitraryTime
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
    , Map <<< EJsonMap <$> do
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

arbitraryDateTime ∷ Gen.Gen DT.DateTime
arbitraryDateTime = DT.DateTime <$> arbitraryDate <*> arbitraryTime

arbitraryDate ∷ Gen.Gen DT.Date
arbitraryDate = do
  year ← Gen.chooseInt 1950 2050
  month ← Gen.chooseInt 1 12
  day ← Gen.chooseInt 1 31
  pure $ DT.canonicalDate
    (fromMaybe bottom (toEnum year))
    (fromMaybe bottom (toEnum month))
    (fromMaybe bottom (toEnum day))

arbitraryTime ∷ Gen.Gen DT.Time
arbitraryTime = do
  hour ← Gen.chooseInt 0 23
  minute ← Gen.chooseInt 0 59
  second ← Gen.chooseInt 0 59
  pure $ DT.Time
    (fromMaybe bottom (toEnum hour))
    (fromMaybe bottom (toEnum minute))
    (fromMaybe bottom (toEnum second))
    bottom
