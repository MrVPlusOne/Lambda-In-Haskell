module MyOps where

import Prelude
import qualified Control.Monad as M (mplus)

infixl 0 |>
(|>) :: a -> (a->b) -> b
a |> f = f a

infixl 0 />
(/>) :: (a->b) -> (b->c) -> a->c
f /> g = g . f

mplus :: Maybe a -> Maybe a -> Maybe a
mplus = M.mplus

type Mayfail = Either String

good :: a -> Mayfail a
good = Right

wrong :: String -> Mayfail a
wrong = Left

surroundString :: Bool -> String -> String
surroundString cond str
  | cond = "(" ++ str ++ ")"
  | otherwise = str
