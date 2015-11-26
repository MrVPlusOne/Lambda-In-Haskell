module MyOps where

import Prelude
import qualified Data.Generics.Aliases as A (orElse)

infixl 0 |>
(|>) :: a -> (a->b) -> b
a |> f = f a

infixl 0 />
(/>) :: (a->b) -> (b->c) -> a->c
f /> g = g . f

orElse :: Maybe a -> Maybe a -> Maybe a
orElse = A.orElse
