module Playground where

import MyOps
import LambdaTerm
import BetaReduce
import Parse
import Control.Applicative
import Data.Maybe (fromMaybe)

zero = parseExpr "λf x. x"
one = parseExpr "λf x. f x"
add = parseExpr "λm n f x. m f(n f x)"
mult = parseExpr "λm n f x. m(n f)x"

next = reduce $ add # one

l = parseExpr "λu x y z. x y z u"
half = lambda "v" (l#(_v#_v))
m = half # half

result = tryReduceInSteps 15 $ _x # _y # _z # m

result2 = reduce $ m # _x # _y # _z

reduce x = fromMaybe (error "can't reduce") (tryReduceInSteps 25 x)
