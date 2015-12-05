# Lambda-In-Haskell
### A haskell library for lambda calculus.

This is a simple library I wrote while learning Type theory and Haskell.

I wish this can help you.

## Usage and examples:

First, `cd` into the project folder and load *Playground.hs* into ghci:

```
cd Path-To-Project
$ ghci Playground.hs
```

This *Playground.hs* file will import those interesting functions exported by other modules for you.
Then you can try with those functions:

#### 1. Untyped lambda calculus

Untyped lambda calculus are the foundation of this project.

* You can parse lambda terms

`ghci> let e1 = parseExpr "λf x. f y x"`

```
ghci> :t e1
e1 :: Term  -- 'Term' is the type of a lambda term
ghci> e1
λf x. f y x -- pretty print of a term
```

* Then you can find out free variables

```
ghci> freeVars e1
fromList ["y"]  -- A set of variable names is returned
```

* You can substitute a variable by another term use `/:` or `substitude`:

```
ghci> ("y", parseExpr "u v") /: e1  -- this will replace all free occurrence of `y` in `e1` by `u v`
λf x. f (u v) x
```

```
ghci> ("y", parseExpr "λ y. x") /: (parseExpr "λ x. x y")
λu. u (λy. x) -- notice that the outer variable `x` has been renamed to `u` automatically.
```

* Find all subterms

```
ghci> subTerms it
fromList [f,u,v,x,f (u v),u v,f (u v) x,λf x. f (u v) x,λx. f (u v) x]
```

* Lambda calculus isn't of much use if it can't *calculate*. So we need *beta-reduction*.

```
ghci> tryReduceInSteps 10 (parseExpr "(λ x y. y x) u v")  -- let's flip u v
Just v u -- you get the result!
```

But not all terms have *beta-normal forms*, that's why the result type is a `Maybe Term`, and you must *tryReduceInSteps*.

For example, the famous term Ω has an infinite reduction sequence:

```
ghci> omega
(λx. x x) (λx. x x)

ghci> tryReduceInSteps 10 omega
Nothing
```

By the way, you can enter the above term like this:

```
ghci> (lambda "x" (Var "x")) # (lambda "x" (Var "x"))  -- '#' is the 'apply' operator
(λx. x) (λx. x)
```

*tryReduceInSteps* will search through all possible reduction paths trying to get a normal-form result.

So, term like '(λx. y) Ω' can be reduced:

```
ghci> let e2 = lambda "x" (Var "y") # omega
ghci> tryReduceInSteps 10 e2
Just y
ghci> reduceChoices e2  -- list all possible β-reduction paths
[y,(λx. y) ((λx. x x) (λx. x x))]
```

* You can also use `betaEqual` to check β-equivalence.

#### 2. Simply typed lambda calculus

Simply typed lambda is provided by 'TypedTerm' module

* Just use `inferThenShow` to type a term

```
ghci> putStrLn $ inferThenShow e1  -- remember e1 == λf x. f y x
λf: t2 -> t1 -> t0. λx: t1. {f: t2 -> t1 -> t0} {y: t2} {x: t1} : (t2 -> t1 -> t0) -> t1 -> t0
ghci> parseinfer "λx y . x(λz . y)y"
λx: (t1 -> t2) -> t2 -> t0. λy: t2. {x: (t1 -> t2) -> t2 -> t0} (λz: t1. {y: t2}) {y: t2} : ((t1 -> t2) -> t2 -> t0) -> t2 -> t0
```

That gives back every variable's type. If you don't want this string result, use `inferType` instead.

```
ghci> parseinfer "λx y . x(λz . x)y"
can't construct infinite type: t8 = (t7 -> t8) -> t5 -> t4
	in x
	in x (λz. x)
	in x (λz. x) y
	in λy. x (λz. x) y
	in λx y. x (λz. x) y
```
