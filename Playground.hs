module Playground where

import MyOps
import LambdaTerm
import BetaReduce
import Parse
import Control.Applicative
import Data.Maybe (fromMaybe)
import TypedTerm


zero = parseExpr "λf x. x"
one = parseExpr "λf x. f x"
add = parseExpr "λm n f x. m f(n f x)"
mult = parseExpr "λm n f x. m(n f)x"

next = reduce $ add # one

l = parseExpr "λu x y z. x y z u"
half = lambda "v" (l#(_v#_v))
m = half # half

result = parseInffer "λx y z . y(x z)x"

parseInffer = putStrLn . infferThenShow . parseExpr

reduce x = fromMaybe (error "can't reduce") (tryReduceInSteps 25 x)
