module BetaReduce
    (
      isBnFrom
    ) where

import MyOps
import LambdaTerm
import Data.List (find)

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


reduceOutter :: Term -> Maybe Term
reduceOutter (Apply (Abstr v p) q) = Just ((v,q) /: p)
reduceOutter _ = Nothing

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
  direct ++ (flip Apply b <$> reduceChoices a) ++ (Apply a <$> reduceChoices b)
  where direct = case reduceOutter t of
                  Just x -> [x]
                  Nothing -> []
reduceChoices (Abstr v p) = Abstr v <$> reduceChoices p
reduceChoices _ = []

{- | Use `reduceChoices` repeatedly until one λ-nf be found or max steps tried out.
prop> try50 (λ "x" (_x # _y) # λ "u" (_v # _u # _u)) == Just (_v # _y # _y)
prop> try50 (λs ["x","y"] (_y # _x) # _u # _v) == Just (_v # _u)
prop> let expr = (λ "x" (_x # (_x # (_y # _z)) # _x)) # (λ "u" (_u # _v)) in try50 expr == Just (_y # _z # _v # _v # λ "u" (_u # _v))
-}
tryReduceInSteps :: Int -> Term -> Maybe Term
tryReduceInSteps s t = loop s [t] where
  loop n _ | n < 0 = Nothing
  loop n ts =
    find isBnFrom ts `orElse` loop (n-1) (ts>>= reduceChoices)

try50 = tryReduceInSteps 50

double :: Term
double = λ "x" (_x # _x)

omega :: Term
omega = double # double

cond = \t -> (null . reduceChoices) t -- == isBnFrom t
result = let n = _u in try50 (λ "x" (_x # (_x # _y)) # n) == Just (n # (n # _y))
