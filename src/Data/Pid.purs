module Data.Pid (
  Identifier,
  Pid,
  PidConstructionError(..),
  new,
  identifiers,
  generate,
  pos,
  site
) where

import Data.Array (zip)
import Data.ArrayExtensions (splitWhere)
import Data.Bounded (top, bottom)
import Data.Either (Either(..), fromRight)
import Data.Eq (class Eq)
import Data.Foldable (all)
import Data.Functor ((<$>))
import Data.HeytingAlgebra ((&&))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Ord (class Ord, (<), (>))
import Data.Ring ((-), (+))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)
import Random.LCG (Seed)
import Random.PseudoRandom (random, randomR)

type Identifier = Tuple Int Int
newtype Pid = Pid (Array Identifier)

instance showPid ∷ Show Pid where
  show (Pid ids) =
    "(Pid " <> show ids <> ")"

derive instance newtypePid ∷ Newtype Pid _
derive instance eqPid ∷ Eq Pid
derive instance ordPid :: Ord Pid

data PidConstructionError = PidOutOfBounds

instance showPidConstructionError ∷ Show PidConstructionError where
  show PidOutOfBounds = "PidOutOfBounds"

derive instance eqPidConstructionError ∷ Eq PidConstructionError

new ∷ Array Identifier → Either PidConstructionError Pid
new ids =
  if all inBounds ids then
    Right (Pid ids)
  else
    Left PidOutOfBounds
  where
    inBounds ∷ Identifier → Boolean
    inBounds (Tuple p s) =
      bottom < p && p < top &&
      bottom < s && s < top

identifiers ∷ Pid → Array Identifier
identifiers = unwrap

pos ∷ Identifier → Int
pos = fst

site ∷ Identifier → Int
site = snd

generate ∷ Pid → Pid → Int → Seed → Pid
generate lower upper mySite seed =
  let
    pairs = zip (identifiers lower) (identifiers upper)
    { left, curr, right } = splitWhere pairs (\pair → diffPos pair > 1)
  in
    case curr of
      Nothing →
        let
          newId = Tuple (random seed).newVal mySite
        in
          mkPid ((fst <$> left) <> [newId])
      Just (Tuple l u) →
        let
          interval = pos u - pos l - 1
          leftLower = fst <$> left
          newPos = pos l + (randomR 1 interval seed).newVal
          newId = Tuple newPos mySite
        in
          mkPid (leftLower <> [newId])
  where
    diffPos (Tuple l u) = pos u - pos l
    mkPid ids = unsafePartial (fromRight (new ids))
