module Data.Matrix
  ( Matrix
  , empty
  , singleton
  , replicate
  , fromArray
  , null
  , index
  , updateAt
  , modifyAt
  , filter
  , zipWith
  , zipWithA
  , catMaybes
  ) where

import Prelude

import Data.Array (length)
import Data.Array as A
import Data.Foldable (class Foldable, foldMap, foldl, foldr, minimum)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Primitivizable (class Primitivizable, evolve, primitivize)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)

data Matrix a = Matrix Int (Array a)

derive instance eqMatrix ∷ Eq a ⇒ Eq (Matrix a)
derive instance functorMatrix ∷ Functor Matrix

instance foldableMatrix ∷ Foldable Matrix where
  foldr f b (Matrix _ m) = foldr f b m
  foldl f b (Matrix _ m) = foldl f b m
  foldMap f (Matrix _ m) = foldMap f m

instance traversableMatrix ∷ Traversable Matrix where
  traverse f (Matrix w m) = Matrix w <$> traverse f m
  sequence (Matrix w m) = Matrix w <$> sequence m

instance functorWithIndexMatrix ∷ FunctorWithIndex {x ∷ Int, y ∷ Int} Matrix
  where
    mapWithIndex f (Matrix w m) =
      Matrix w $ mapWithIndex (\i → f {x: i `mod` w, y: (i / w)}) m

instance foldableWithIndexMatrix
  ∷ FoldableWithIndex {x ∷ Int, y ∷ Int} Matrix
  where
    foldlWithIndex f b (Matrix w m) =
                    foldlWithIndex (\i → f {x: i `mod` w, y: (i / w)}) b m
    foldrWithIndex f b (Matrix w m) =
                    foldrWithIndex (\i → f {x: i `mod` w, y: (i / w)}) b m
    foldMapWithIndex f (Matrix w m) =
                    foldMapWithIndex (\i → f {x: i `mod` w, y: (i / w)}) m

instance traversableWithIndexMatrix
  ∷ TraversableWithIndex {x ∷ Int, y ∷ Int} Matrix
  where
    traverseWithIndex f (Matrix w m) =
      Matrix w <$> traverseWithIndex (\i → f {x: i `mod` w, y: (i / w)}) m

instance primitivizableMatrix
   ∷ Primitivizable a b
  ⇒ Primitivizable (Matrix a) {width ∷ Int, values ∷ Array b} where
    primitivize (Matrix w m) = {width: w, values: primitivize m}
    evolve {width, values} = Matrix width $ evolve values


empty ∷ ∀ a. Matrix a
empty = Matrix 0 []

singleton ∷ ∀ a. a → Matrix a
singleton a = Matrix 1 [a]

replicate ∷ ∀ a. Int → Int → a → Matrix a
replicate w h a = Matrix w $ A.replicate (w * h) a

fromArray ∷ ∀ a. Array (Array a) → Matrix a
fromArray a =
  let w = fromMaybe 0 $ minimum $ A.length <$> a
  in if w > 0 then Matrix w $ A.concat $ A.take w <$> a
              else empty

null ∷ ∀ a. Matrix a → Boolean
null (Matrix _ m) = A.null m

index ∷ ∀ a. Int → Int → Matrix a → Maybe a
index x y (Matrix w m) = if 0 <= x && x < w then m A.!! (y * w + x)
                                            else Nothing

updateAt ∷ ∀ a. Int → Int → a → Matrix a → Maybe (Matrix a)
updateAt x y a (Matrix w m) =
  if x < w then Matrix w <$> A.updateAt (y * w + x) a m else Nothing

modifyAt ∷ ∀ a. Int → Int → (a → a) → Matrix a → Maybe (Matrix a)
modifyAt x y f (Matrix w m) =
  if x < w then Matrix w <$> A.modifyAt (y * w + x) f m else Nothing

filter ∷ ∀ a. (a → Boolean) → Matrix a → Array a
filter f (Matrix _ m) = A.filter f m

zipWith ∷ ∀ a b c. (a → b → c) → Matrix a → Matrix b → Matrix c
zipWith f (Matrix wa ma) (Matrix wb mb) =
  let
    wc = min wa wb
    hc = min (length ma / wa) (length mb / wb)
    fc = \i →
      let cx = i `mod` wc
          cy = i / wc
      in  f <$> ma A.!! (cy * wa + cx)
            <*> mb A.!! (cy * wb + cx)
    mc = A.catMaybes $ fc <$> 0 A... (wc * hc - 1)
  in
    Matrix wc mc

zipWithA
   ∷ ∀ m a b c
   . Applicative m
  ⇒ (a → b → m c)
  → Matrix a
  → Matrix b
  → m (Matrix c)
zipWithA f ma mb = sequence $ zipWith f ma mb

catMaybes ∷ ∀ a. Matrix (Maybe a) → Array a
catMaybes (Matrix _ a) = A.catMaybes a
