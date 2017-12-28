module Control.Applicative.Acme where

{- |
Application with the inverse lifting behavior of fmap.

Issue #1
-}
pamf :: Functor f => f (a -> b) -> a -> f b
pamf f a = fmap ($ a) f

{- |
Operator version of pamf

Issue #1
-}
(>$<) :: Functor f => f (a -> b) -> a -> f b
(>$<) = pamf
