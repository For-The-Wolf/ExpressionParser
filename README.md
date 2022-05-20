# ExpressionParser
A Haskell library for parsing and analysing expressions - old project, written as a learning exercise when exploring Haskell, functional design patterns, category theory etc.

Expressions are represented through the `Expr` type - a recursive monadic structure which is a varation of the Tree monad. The monadic bind operation `>>=` essentially performs a variable substitution.

Expressions can be simplified, sustituted into one another, partially differentiated, and evaluated.

## Usage
Build both the library and executable by running either `cabal build` or `cabal new-build` from within the directory.
Run the executable similarly, with either `cabal run` or `cabal new-run`.

## Example executable
Allows the user to type in a plain text function or expression. This is parsed, simplified and differentiated twice. A variable can then be subsituted for and the expressions are all further simplified.

Example output of running this executable is shown below:
```
λ  ExpressionParser git:(main) cabal new-run  
Up to date
------------------------------------
Input a function or expression:
f(x)=(5*x*x)-(10*x)+23
f(x) = ((5.0*(x*x))-((10.0*x)+23.0))
f'(x) = (((2.0*x)*5.0)-10.0)
f''(x) = 10.0
What would you like to substitute for x?
0.5
f(0.5) = -26.75
f'(0.5) = -5.0
f''(0.5) = 10.0
------------------------------------
Input a function or expression:
stop
Exiting...
------------------------------------
```
