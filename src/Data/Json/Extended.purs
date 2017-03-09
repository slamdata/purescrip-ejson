module Data.Json.Extended
  ( module Exports

  , EJson(..)

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

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array as A
import Data.Bitraversable (bitraverse)
import Data.Bifunctor (lmap)
import Data.Either as E
import Data.Eq (eq1)
import Data.Functor.Mu as Mu
import Data.Functor.Coproduct (Coproduct)
import Data.HugeNum as HN
import Data.Json.Extended.Signature as Sig
import Data.Json.Extended.Type (EJsonType)
import Data.Lens (Prism', preview, prism')
import Data.Map as Map
import Data.Maybe as M
import Data.Newtype as N
import Data.Ord (compare1)
import Data.StrMap as SM
import Data.Traversable (for)
import Data.Tuple as T

import Matryoshka (class Corecursive, class Recursive, embed, project, cata, anaM)

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen
import Text.Parsing.Parser as P

import Data.Json.Extended.Signature hiding (getType) as Exports

newtype EJson = EJson (Mu.Mu Sig.EJsonF)

derive instance newtypeEJson :: N.Newtype EJson _

instance recursiveEJson ∷ Recursive EJson Sig.EJsonF where
  project = N.traverse EJson project

instance corecursiveEJson ∷ Corecursive EJson Sig.EJsonF where
  embed = N.collect EJson embed

derive newtype instance eqEJson ∷ Eq EJson
derive newtype instance ordEJson ∷ Ord EJson

instance showEJson ∷ Show EJson where
  show = renderEJson

instance decodeJsonEJson ∷ DecodeJson EJson where
  decodeJson = anaM Sig.decodeJsonEJsonF

-- | This is a _lossy_ encoding of EJSON to JSON; JSON only supports objects with strings
-- as keys.
instance encodeJsonEJson ∷ EncodeJson EJson where
  encodeJson = cata Sig.encodeJsonEJsonF


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

renderEJson
  ∷ EJson
  → String
renderEJson =
  cata Sig.renderEJsonF


-- | A closed parser of SQL^2 constant expressions
parseEJson
  ∷ forall m
  . (Monad m)
  ⇒ P.ParserT String m EJson
parseEJson =
  Lazy.fix \f →
    embed <$>
      Sig.parseEJsonF f


null ∷ EJson
null = embed Sig.Null

boolean ∷ Boolean → EJson
boolean = embed <<< Sig.Boolean

integer ∷ Int → EJson
integer = embed <<< Sig.Integer

decimal ∷ HN.HugeNum → EJson
decimal = embed <<< Sig.Decimal

string ∷ String → EJson
string = embed <<< Sig.String

timestamp ∷ String → EJson
timestamp = embed <<< Sig.Timestamp

date ∷ String → EJson
date = embed <<< Sig.Date

time ∷ String → EJson
time = embed <<< Sig.Time

interval ∷ String → EJson
interval = embed <<< Sig.Interval

objectId ∷ String → EJson
objectId = embed <<< Sig.ObjectId

array ∷ Array EJson → EJson
array = embed <<< Sig.Array

map ∷ Map.Map EJson EJson → EJson
map = embed <<< Sig.Map <<< A.fromFoldable <<< Map.toList

map' ∷ SM.StrMap EJson → EJson
map' = embed <<< Sig.Map <<< F.map go <<< A.fromFoldable <<< SM.toList
  where
    go (T.Tuple a b) = T.Tuple (string a) b

getType ∷ EJson → EJsonType
getType = Sig.getType <<< project

_Null ∷ Prism' EJson Unit
_Null = prism' (const null) $ project >>> case _ of
  Sig.Null → M.Just unit
  _ → M.Nothing

_String ∷ Prism' EJson String
_String = prism' string $ project >>> case _ of
  Sig.String s → M.Just s
  _ → M.Nothing

_Boolean ∷ Prism' EJson Boolean
_Boolean = prism' boolean $ project >>> case _ of
  Sig.Boolean b → M.Just b
  _ → M.Nothing

_Integer ∷ Prism' EJson Int
_Integer = prism' integer $ project >>> case _ of
  Sig.Integer i → M.Just i
  _ → M.Nothing

_Decimal ∷ Prism' EJson HN.HugeNum
_Decimal = prism' decimal $ project >>> case _ of
  Sig.Decimal d → M.Just d
  _ → M.Nothing

_Timestamp ∷ Prism' EJson String
_Timestamp = prism' timestamp $ project >>> case _ of
  Sig.Timestamp t → M.Just t
  _ → M.Nothing

_Date ∷ Prism' EJson String
_Date = prism' date $ project >>> case _ of
  Sig.Date d → M.Just d
  _ → M.Nothing

_Time ∷ Prism' EJson String
_Time = prism' time $ project >>> case _ of
  Sig.Time t → M.Just t
  _ → M.Nothing

_Interval ∷ Prism' EJson String
_Interval = prism' interval $ project >>> case _ of
  Sig.Interval i → M.Just i
  _ → M.Nothing

_ObjectId ∷ Prism' EJson String
_ObjectId = prism' objectId $ project >>> case _ of
  Sig.ObjectId id → M.Just id
  _ → M.Nothing

_Array ∷ Prism' EJson (Array EJson)
_Array = prism' array $ project >>> case _ of
  Sig.Array xs → M.Just xs
  _ → M.Nothing

_Map ∷ Prism' EJson (Map.Map EJson EJson)
_Map = prism' map $ project >>> case _ of
  Sig.Map kvs → M.Just $ Map.fromFoldable kvs
  _ → M.Nothing

_Map' ∷ Prism' EJson (SM.StrMap EJson)
_Map' = prism' map' $ project >>> case _ of
  Sig.Map kvs → SM.fromFoldable <$> for kvs (bitraverse (preview _String) pure)
  _ → M.Nothing
