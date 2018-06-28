module Data.Json.Extended
  ( module Exports
  , EJson
  , null
  , boolean
  , integer
  , decimal
  , number
  , string
  , map
  , array
  , renderEJson
  , parseEJson
  , arbitraryEJsonOfSize
  , getType
  , _Null
  , _String
  , _Boolean
  , _Integer
  , _Decimal
  , _Number
  , _Array
  , _Map
  ) where

import Prelude hiding (map)

import Control.Lazy as Lazy
import Control.Monad.Gen (class MonadGen)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either as E
import Data.Functor.Mu as Mu
import Data.HugeInt as HI
import Data.HugeNum as HN
import Data.Json.Extended.Signature as Sig
import Data.Json.Extended.Signature hiding (getType) as Exports
import Data.Json.Extended.Type (EJsonType)
import Data.Lens (Prism', prism')
import Data.Map as Map
import Data.Maybe as M
import Matryoshka (class Corecursive, class Recursive, anaM, cata, embed, project)
import Text.Parsing.Parser as P

type EJson = Mu.Mu Sig.EJsonF

arbitraryEJsonOfSize
  ∷ ∀ m t
  . MonadGen m
  ⇒ MonadRec m
  ⇒ Corecursive t Sig.EJsonF
  ⇒ Int
  → m t
arbitraryEJsonOfSize = anaM Sig.arbitraryEJsonF

renderEJson ∷ ∀ t. Recursive t Sig.EJsonF ⇒ t → String
renderEJson = cata Sig.renderEJsonF

parseEJson ∷ ∀ m. Monad m ⇒ P.ParserT String m EJson
parseEJson =
  Lazy.fix \f → embed <$> Sig.parseEJsonF f

null ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ t
null = embed Sig.Null

boolean ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ Boolean → t
boolean = embed <<< Sig.Boolean

integer ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ HI.HugeInt → t
integer = embed <<< Sig.Integer

decimal ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ HN.HugeNum → t
decimal = embed <<< Sig.Decimal

number ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ E.Either HI.HugeInt HN.HugeNum → t
number = embed <<< E.either Sig.Integer Sig.Decimal

string ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ String → t
string = embed <<< Sig.String

array ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ Array t → t
array = embed <<< Sig.Array

map ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ Map.Map t t → t
map = embed <<< Sig.Map <<< Sig.EJsonMap <<< Map.toUnfoldable

getType ∷ ∀ t. Recursive t Sig.EJsonF ⇒ t → EJsonType
getType = Sig.getType <<< project

_Null ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ Recursive t Sig.EJsonF ⇒ Prism' t Unit
_Null = prism' (const null) $ project >>> case _ of
  Sig.Null → M.Just unit
  _ → M.Nothing

_String ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ Recursive t Sig.EJsonF ⇒ Prism' t String
_String = prism' string $ project >>> case _ of
  Sig.String s → M.Just s
  _ → M.Nothing

_Boolean ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ Recursive t Sig.EJsonF ⇒ Prism' t Boolean
_Boolean = prism' boolean $ project >>> case _ of
  Sig.Boolean b → M.Just b
  _ → M.Nothing

_Integer ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ Recursive t Sig.EJsonF ⇒ Prism' t HI.HugeInt
_Integer = prism' integer $ project >>> case _ of
  Sig.Integer i → M.Just i
  _ → M.Nothing

_Decimal ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ Recursive t Sig.EJsonF ⇒ Prism' t HN.HugeNum
_Decimal = prism' decimal $ project >>> case _ of
  Sig.Decimal d → M.Just d
  _ → M.Nothing

_Number ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ Recursive t Sig.EJsonF ⇒ Prism' t (E.Either HI.HugeInt HN.HugeNum)
_Number = prism' number $ project >>> case _ of
  Sig.Integer i → M.Just (E.Left i)
  Sig.Decimal d → M.Just (E.Right d)
  _ → M.Nothing

_Array ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ Recursive t Sig.EJsonF ⇒ Prism' t (Array t)
_Array = prism' array $ project >>> case _ of
  Sig.Array xs → M.Just xs
  _ → M.Nothing

_Map ∷ ∀ t. Corecursive t Sig.EJsonF ⇒ Recursive t Sig.EJsonF ⇒ Ord t ⇒ Prism' t (Map.Map t t)
_Map = prism' map $ project >>> case _ of
  Sig.Map (Sig.EJsonMap kvs) → M.Just $ Map.fromFoldable kvs
  _ → M.Nothing
