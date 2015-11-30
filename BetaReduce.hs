module BetaReduce
    (
      isBnFrom
    ) where

import MyOps
import LambdaTerm
import Data.List (find)
import Parse

{- $setup
>>> import MyOps
>>> import LambdaTerm
>>> import Test.QuickCheck
-}

-- | whether the term is in Beta-normal form
-- >>> isBnFrom (_x # (_x # _y))
-- True
--
-- >>> isBnFrom ((lambda "a" _u) # (_x # _y))
-- False
isBnFrom :: Term -> Bool
isBnFrom = patternMatch f /> null
  where f (Apply (Abstr _ _) _) = Just ()
        f _ = Nothing


reduceOutter :: Term -> [Term]
reduceOutter (Apply (Abstr v p) q) = [(v,q) /: p]
reduceOutter _ = []

{- | List all possible next steps to beta-reduce the term
prop> head (λ "x" (_x # (_x # _y)) # n |> reduceChoices) == n#(n#_y)
prop> _y == head ((λ "x" _y) # n |> reduceChoices)
prop> \t -> (null . reduceChoices) t == isBnFrom t

>>> λ "x" (λ "y" (_y # _x) # _z) # _v |> reduceChoices |> length |> (==2)
True

>>> ((λ "x" (λ "y" (_y # _x) # _z) # _v |> reduceChoices) >>= reduceChoices) == [_z# _v, _z# _v]
True

-}
reduceChoices :: Term -> [Term]
reduceChoices t@(Apply a b) =
  reduceOutter t ++ (flip Apply b <$> reduceChoices a) ++ (Apply a <$> reduceChoices b)
reduceChoices (Abstr v p) = Abstr v <$> reduceChoices p
reduceChoices _ = []

{- | Use `reduceChoices` repeatedly until one λ-nf be found or max steps tried out.
prop> parseReduce "(λx.x y)(λu.v u u)" == Just (parseExpr "v y y")
prop> parseReduce "(λx. x(x(y z))x)(λu.u v)" == Just (parseExpr "y z v v (λu. u v)")
prop> parseReduce "(λx y. y x)u v" == Just (_v # _u)
prop> parseReduce "(λx y z. x z(y z))((λx y. y x)u)((λx y. y x)v) w" == Just (parseExpr "w u(w v)")
-}
tryReduceInSteps :: Int -> Term -> Maybe Term
tryReduceInSteps s t = loop s [t] where
  loop n _ | n < 0 = Nothing
  loop n ts =
    find isBnFrom ts `mplus` loop (n-1) (ts>>= reduceChoices)

try50 :: Term -> Maybe Term
try50 = tryReduceInSteps 50

parseReduce :: String -> Maybe Term
parseReduce = try50 . parseExpr

parseReducePrint s =
  print $ case parseReduce s of
    Just r -> prettyShow r
    Nothing -> "can't reduce in 50 steps"

double :: Term
double = λ "x" (_x # _x)

omega :: Term
omega = double # double

fixedPointCombinator :: Term
fixedPointCombinator = parseExpr "λy. (λx. y(x x))(λx. y(x x))"

result = (fixedPointCombinator # l)
  where l = parseExpr "λy. (λx. x y x)"
