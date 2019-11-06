module Data.UniqueList
  ( UniqueList
  , empty
  , append
  , toArray
  ) where

import Data.Newtype (class Newtype, unwrap)
import Random.LCG (Seed)
import Data.Monoid ((<>))

newtype UniqueList a = UniqueList (Array a)

derive instance newtypeUniqueList ∷ Newtype (UniqueList a) _

empty ∷ ∀ a. UniqueList a
empty = UniqueList []

append ∷ ∀ a. UniqueList a → a → Int → Seed → UniqueList a
append (UniqueList items) item site seed =
  UniqueList (items <> [item])

toArray ∷ ∀ a. UniqueList a → Array a
toArray = unwrap