module Data.Json.Extended
  ( module Exports
  , EJson
  , null
  , boolean
  , integer
  , decimal
  , string
  , timestamp
  , date
  , time
  , interval
  , objectId
  , map
  , map'
  , array

  , renderEJson
  , parseEJson
  , decodeEJson
  , encodeEJson

  , arbitraryEJsonOfSize
  , arbitraryJsonEncodableEJsonOfSize

  , getType

  , _Null
  , _String
  , _Boolean
  , _Integer
  , _Decimal
  , _Timestamp
  , _Date
  , _Time
  , _Interval
  , _ObjectId
  , _Array
  , _Map
  , _Map'
  ) where

import Prelude hiding (map)

import Data.Functor as F

import Control.Lazy as Lazy

import Data.Argonaut as JS
import Data.Array as A
import Data.Bitraversable (bitraverse)
import Data.DateTime as DT
import Data.Either as E
import Data.Functor.Mu as Mu
import Data.HugeNum as HN
import Data.Json.Extended.Signature as Sig
import Data.Json.Extended.Type (EJsonType)
import Data.Lens (Prism', preview, prism')
import Data.Map as Map
import Data.Maybe as M
import Data.StrMap as SM
import Data.Traversable (for)
import Data.Tuple as T

import Matryoshka (class Corecursive, class Recursive, anaM, cata, embed, project)

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen
import Text.Parsing.Parser as P

import Data.Json.Extended.Signature hiding (getType) as Exports

type EJson = Mu.Mu Sig.EJsonF

decodeEJson :: forall t. Corecursive t Sig.EJsonF ⇒ JS.Json → E.Either String t
decodeEJson = anaM Sig.decodeJsonEJsonF

encodeEJson :: forall t. Recursive t Sig.EJsonF ⇒ t -> JS.Json
encodeEJson = cata Sig.encodeJsonEJsonF

arbitraryEJsonOfSize
  ∷ Gen.Size
  → Gen.Gen EJson
arbitraryEJsonOfSize size =
  embed <$>
    case size of
      0 → Sig.arbitraryBaseEJsonF
      n → Sig.arbitraryEJsonF $ arbitraryEJsonOfSize (n - 1)

-- | Generate only JSON-encodable objects
arbitraryJsonEncodableEJsonOfSize
  ∷ Gen.Size
  → Gen.Gen EJson
arbitraryJsonEncodableEJsonOfSize size =
  embed <$>
    case size of
      0 → Sig.arbitraryBaseEJsonF
      n → Sig.arbitraryEJsonFWithKeyGen keyGen $ arbitraryJsonEncodableEJsonOfSize (n - 1)
  where
    keyGen =
      embed <<< Sig.String <$>
        SC.arbitrary

renderEJson ∷ EJson → String
renderEJson =
  cata Sig.renderEJsonF

-- | A closed parser of SQL^2 constant expressions
parseEJson ∷ ∀ m. Monad m ⇒ P.ParserT String m EJson
parseEJson =
  Lazy.fix \f →
    embed <$>
      Sig.parseEJsonF f

null ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ t
null = embed Sig.Null

boolean ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ Boolean → t
boolean = embed <<< Sig.Boolean

integer ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ Int → t
integer = embed <<< Sig.Integer

decimal ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ HN.HugeNum → t
decimal = embed <<< Sig.Decimal

string ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ String → t
string = embed <<< Sig.String

timestamp ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ DT.DateTime → t
timestamp = embed <<< Sig.Timestamp

date ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ DT.Date → t
date = embed <<< Sig.Date

time ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ DT.Time → t
time = embed <<< Sig.Time

interval ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ String → t
interval = embed <<< Sig.Interval

objectId ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ String → t
objectId = embed <<< Sig.ObjectId

array ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ Array t → t
array = embed <<< Sig.Array

map ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ Map.Map t t → t
map = embed <<< Sig.Map <<< A.fromFoldable <<< Map.toList

map' ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ SM.StrMap t → t
map' = embed <<< Sig.Map <<< F.map go <<< A.fromFoldable <<< SM.toList
  where
    go (T.Tuple a b) = T.Tuple (string a) b

getType ∷ ∀ t. Recursive t Sig.EJsonF ⇒ t → EJsonType
getType = Sig.getType <<< project

_Null ∷ ∀ t. (Corecursive t Sig.EJsonF, Recursive t Sig.EJsonF) ⇒ Prism' t Unit
_Null = prism' (const null) $ project >>> case _ of
  Sig.Null → M.Just unit
  _ → M.Nothing

_String ∷ ∀ t. (Corecursive t Sig.EJsonF, Recursive t Sig.EJsonF) ⇒ Prism' t String
_String = prism' string $ project >>> case _ of
  Sig.String s → M.Just s
  _ → M.Nothing

_Boolean ∷ ∀ t. (Corecursive t Sig.EJsonF, Recursive t Sig.EJsonF) ⇒ Prism' t Boolean
_Boolean = prism' boolean $ project >>> case _ of
  Sig.Boolean b → M.Just b
  _ → M.Nothing

_Integer ∷ ∀ t. (Corecursive t Sig.EJsonF, Recursive t Sig.EJsonF) ⇒ Prism' t Int
_Integer = prism' integer $ project >>> case _ of
  Sig.Integer i → M.Just i
  _ → M.Nothing

_Decimal ∷ ∀ t. (Corecursive t Sig.EJsonF, Recursive t Sig.EJsonF) ⇒ Prism' t HN.HugeNum
_Decimal = prism' decimal $ project >>> case _ of
  Sig.Decimal d → M.Just d
  _ → M.Nothing

_Timestamp ∷ ∀ t. (Corecursive t Sig.EJsonF, Recursive t Sig.EJsonF) ⇒ Prism' t DT.DateTime
_Timestamp = prism' timestamp $ project >>> case _ of
  Sig.Timestamp t → M.Just t
  _ → M.Nothing

_Date ∷ ∀ t. (Corecursive t Sig.EJsonF, Recursive t Sig.EJsonF) ⇒ Prism' t DT.Date
_Date = prism' date $ project >>> case _ of
  Sig.Date d → M.Just d
  _ → M.Nothing

_Time ∷ ∀ t. (Corecursive t Sig.EJsonF, Recursive t Sig.EJsonF) ⇒ Prism' t DT.Time
_Time = prism' time $ project >>> case _ of
  Sig.Time t → M.Just t
  _ → M.Nothing

_Interval ∷ ∀ t. (Corecursive t Sig.EJsonF, Recursive t Sig.EJsonF) ⇒ Prism' t String
_Interval = prism' interval $ project >>> case _ of
  Sig.Interval i → M.Just i
  _ → M.Nothing

_ObjectId ∷ ∀ t. (Corecursive t Sig.EJsonF, Recursive t Sig.EJsonF) ⇒ Prism' t String
_ObjectId = prism' objectId $ project >>> case _ of
  Sig.ObjectId id → M.Just id
  _ → M.Nothing

_Array ∷ ∀ t. (Corecursive t Sig.EJsonF, Recursive t Sig.EJsonF) ⇒ Prism' t (Array t)
_Array = prism' array $ project >>> case _ of
  Sig.Array xs → M.Just xs
  _ → M.Nothing

_Map ∷ ∀ t. (Corecursive t Sig.EJsonF, Recursive t Sig.EJsonF, Ord t) ⇒ Prism' t (Map.Map t t)
_Map = prism' map $ project >>> case _ of
  Sig.Map kvs → M.Just $ Map.fromFoldable kvs
  _ → M.Nothing

_Map' ∷ ∀ t. (Corecursive t Sig.EJsonF, Recursive t Sig.EJsonF) ⇒ Prism' t (SM.StrMap t)
_Map' = prism' map' $ project >>> case _ of
  Sig.Map kvs → SM.fromFoldable <$> for kvs (bitraverse (preview _String) pure)
  _ → M.Nothing
