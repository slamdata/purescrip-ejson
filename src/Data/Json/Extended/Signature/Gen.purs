module Data.Json.Extended.Signature.Gen
  ( arbitraryEJsonF
  ) where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array as A
import Data.HugeInt as HI
import Data.HugeNum as HN
import Data.Json.Extended.Signature.Core (EJsonF(..), EJsonMap(..))
import Data.NonEmpty ((:|))
import Data.String.Gen as GenS
import Data.Tuple as T
import Matryoshka (CoalgebraM)

arbitraryEJsonF ∷ ∀ m. MonadGen m ⇒ MonadRec m ⇒ CoalgebraM m EJsonF Int
arbitraryEJsonF 0 =
  Gen.oneOf $ pure Null :|
    [ Boolean <$> Gen.chooseBool
    , Integer <<< HI.fromInt <$> Gen.chooseInt (-1000000) 1000000
    , Decimal <<< HN.fromNumber <$> Gen.chooseFloat (-1000000.0) 1000000.0
    , String <$> GenS.genUnicodeString
    ]
arbitraryEJsonF n = do
  len ← Gen.chooseInt 0 $ n - 1
  Gen.oneOf $ arbitraryEJsonF 0 :|
    [ pure $ Array $ A.replicate len $ n - 1
    , pure $ Map $ EJsonMap $ A.replicate len $ T.Tuple (n - 1) (n - 1)
    ]
