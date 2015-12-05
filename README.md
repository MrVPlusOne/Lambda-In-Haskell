# Lambda-In-Haskell
### A haskell library for lamdba calculus.

This is a simple library I wrote while leanring Type theory and Haskell.

## Usage and examples:

First, cd into the project folder and load *Playground.hs* into ghci:

`$ ghci Playground.hs`

This *Playground.hs* file will import those interesting functions exported by other modules for you.
Then you can try with those functions:

#### 1. Untyped lambda calculus

Untyped lambda calculus are the fundation of this project.

* You can parse lambda terms

`ghci> let e1 = parseExpr "λf x. f y x" -- This will give you a LamdbaTerm`

```
ghci> :t e1
e1 :: Term
```

* Then you can find out free variables

```
ghci> freeVars e1
fromList ["y"]  -- A set of varibales names is returned
```

* You can substitue a variable by another term use `/:` or `substitude`:

```
ghci> ("y", parseExpr "u v") /: e1  -- this will replace all free ocurrances of `y` in `e1` by `u v`
λf x. f (u v) x
```

```
ghci> ("y", parseExpr "λ y. x") /: (parseExpr "λ x. x y")
λu. u (λy. x) -- notice that the outter variable `x` has been renamed to `u` automatically.
```

* Find all subterms

```
ghci> subTerms it
fromList [f,u,v,x,f (u v),u v,f (u v) x,λf x. f (u v) x,λx. f (u v) x]
```

* Lambda calculus isn't of much use if it can't *calculate*. So we need *beta-reduction*.

```
ghci> tryReduceInSteps 10 (parseExpr "(λ x y. y x) u v")  -- let's flip u v
Just v u -- you got the result!
```

But not all terms have *beta-normal forms*, that's why the result type is a `Maybe Term`, and you must *tryReduceInSteps*.

For example, the famous term Ω has an infinite reduction sequence:

```
ghci> omega
(λx. x x) (λx. x x)

ghci> tryReduceInSteps 10 omega
Nothing
```

*tryReduceInSteps* will search through all possible reduction paths trying to get a normal form result.

So, term like '(λx. y)Ω' can be reduced:


