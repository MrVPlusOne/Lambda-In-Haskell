module MyOps where

import Prelude  

infixl 0 |>
(|>) :: a -> (a->b) -> b
a |> f = f a

infixl 0 />
(/>) :: (a->b) -> (b->c) -> a->c
f /> g = g . f
