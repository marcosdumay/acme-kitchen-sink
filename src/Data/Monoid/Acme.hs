module Data.Monoid.Acme where


import Control.Applicative

{- |
Many generalization for Monoid instances.

Issue #2
-}
mmany :: (Alternative a, Monoid m) => a m -> a m
mmany f = foldl mappend mempty <$> many f

{- |
Some generatlization for Monoid instances.

Issue #2
-}
msome :: (Alternative a, Monoid m) => a m -> a m
msome f = foldl mappend mempty <$> some f

{- |
Utility for casting bare values into a Monoid in mmany.

For example, the function:

>  let sumMany = manyWith Sum getSum

will sum the results of many.

Issue #2
-}
manyWith :: (Alternative a, Monoid m) => (v -> m) -> (m -> v) -> a v -> a v
manyWith toMonoid fromMonoid f = fromMonoid <$> (mmany $ toMonoid <$> f)

{- |
Utility for casting bare values into a Monoid in msome.

For example, the function:

> let sumSome = someWith Sum getSum

will sum the results of some.

Issue #2
-}
someWith :: (Alternative a, Monoid m) => (v -> m) -> (m -> v) -> a v -> a v
someWith toMonoid fromMonoid f = fromMonoid <$> (msome $ toMonoid <$> f)
