{-# LANGUAGE FlexibleInstances #-}

{- |
Monoids on category composition.

Issue #3
-}
module Data.Monoid.Category where

import qualified Control.Category as Cat
import Data.Monoid.Acme
import Control.Applicative

-- | Monoid on category composition (>>>)
newtype Composition c = Composition {getComposition :: c}
instance Cat.Category c => Monoid (Composition (c a a)) where
  mempty = Composition Cat.id
  mappend (Composition a) (Composition b) = Composition $ a Cat.>>> b


-- | Monoid on reverse category composition (<<<)
newtype RevComposition c = RevComposition {getRevComposition :: c}
instance Cat.Category c => Monoid (RevComposition (c a a)) where
  mempty = RevComposition Cat.id
  mappend (RevComposition a) (RevComposition b) = RevComposition $ a Cat.<<< b

-- | Alternatives accumulator @many@ for Composition
composeMany :: (Alternative m, Cat.Category cat) => m (cat a a) -> m (cat a a)
composeMany = manyWith Composition getComposition

-- | Alternatives accumulator @many@ for RevComposition
invComposeMany :: (Alternative m, Cat.Category cat) => m (cat a a) -> m (cat a a)
invComposeMany = manyWith RevComposition getRevComposition
