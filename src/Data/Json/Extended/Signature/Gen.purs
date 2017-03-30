module Data.Json.Extended.Signature.Gen
  ( arbitraryEJsonF
  ) where

import Prelude

import Data.Array as A
import Data.DateTime as DT
import Data.Enum (toEnum)
import Data.Int as Int
import Data.HugeNum as HN
import Data.Json.Extended.Signature.Core (EJsonF(..), EJsonMap(..))
import Data.Maybe (fromMaybe)
import Data.Tuple as T

import Matryoshka (CoalgebraM)

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

arbitraryEJsonF ∷ CoalgebraM Gen.Gen EJsonF Int
arbitraryEJsonF 0 =
  Gen.oneOf (pure Null)
    [ map Boolean $ SC.arbitrary
    , map Integer $ SC.arbitrary
    , map Decimal $ arbitraryDecimal
    , map String $ arbitraryString
    , map Timestamp $ arbitraryDateTime
    , map Date $ arbitraryDate
    , map Time $ arbitraryTime
    , map Interval $ map ("P" <> _) $ SC.arbitrary
    , map ObjectId arbitraryObjectId
    ]
arbitraryEJsonF n = do
  len ← Gen.chooseInt 0 $ n - 1
  Gen.oneOf (arbitraryEJsonF 0)
    [ pure $ Array $ A.replicate len $ n - 1
    , pure $ Map $ EJsonMap $ A.replicate len $ T.Tuple (n - 1) (n - 1)
    ]

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


arbitraryObjectId ∷ Gen.Gen String
arbitraryObjectId =
  Int.toStringAs Int.hexadecimal <$> SC.arbitrary

arbitraryString ∷ Gen.Gen String
arbitraryString =
  Int.toStringAs Int.base36 <$> SC.arbitrary
