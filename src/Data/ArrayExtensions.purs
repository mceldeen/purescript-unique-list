module Data.ArrayExtensions where

import Data.Array (snoc, uncons)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), fromMaybe)

splitWhere ∷
  ∀ a
  . Array a
  → (a → Boolean)
  → { left ∷ Array a, curr ∷ Maybe a, right ∷ Array a }
splitWhere items pred =
  go { left : [], curr : Nothing, right : items }
  where
    go { left, curr, right } =
      case uncons right of
        Nothing →
          { left : fromMaybe left (snoc left <$> curr), curr : Nothing, right }
        Just { head, tail } →
          let
            next =
              { left : fromMaybe left (snoc left <$> curr)
              , curr : Just head
              , right : tail
              }
          in
            if pred head then next else go next