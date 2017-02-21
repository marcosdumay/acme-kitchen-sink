module Control.Applicative.Acme where

{- |
Application with the inverse lifting behavior of fmap.

Issue #1
-}
pamf :: Applicative f => f (a -> b) -> a -> f b
pamf f a = f <*> pure a

{- |
Operator version of pamf

Issue #1
-}
(>$<) :: Applicative f => f (a -> b) -> a -> f b
(>$<) = pamf
