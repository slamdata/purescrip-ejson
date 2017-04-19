module Data.Json.Extended.Signature.Gen
  ( arbitraryEJsonF
  ) where

import Prelude

import Data.Array as A
import Data.HugeNum as HN
import Data.Json.Extended.Signature.Core (EJsonF(..), EJsonMap(..))
import Data.Tuple as T

import Matryoshka (CoalgebraM)

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

arbitraryEJsonF ∷ CoalgebraM Gen.Gen EJsonF Int
arbitraryEJsonF 0 =
  Gen.oneOf (pure Null)
    [ map Boolean SC.arbitrary
    , map Integer SC.arbitrary
    , map Decimal $ map HN.fromNumber SC.arbitrary
    , map String SC.arbitrary
    ]
arbitraryEJsonF n = do
  len ← Gen.chooseInt 0 $ n - 1
  Gen.oneOf (arbitraryEJsonF 0)
    [ pure $ Array $ A.replicate len $ n - 1
    , pure $ Map $ EJsonMap $ A.replicate len $ T.Tuple (n - 1) (n - 1)
    ]
