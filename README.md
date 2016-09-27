# mikrokosmos

**Mikrokosmos** is an untyped lambda calculus interpreter, borrowing its name from the series of
progressive piano études *[Mikrokosmos](https://www.youtube.com/watch?v=VEsMk3DAzWM)* written by *Bela Bartok*. 
It aims to provide students with a tool to learn and understand lambda calculus.
If you want to start learning about lambda calculus, I suggest you to read:

 * [The wikipedia page on Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus#Informal_description)
 * [A tutorial introduction to the Lambda calculus by Raúl Rojas](www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf)

And to install and to tinker with this interpreter.

## Installation

Mikrokosmos is installable from [Hackage](http://hackage.haskell.org/); you can install it directly from `cabal`: 
```
cabal update
cabal install mikrokosmos
```

However, you can also install it by cloning the git repository and using [cabal](https://www.haskell.org/cabal/):

``` bash
git clone https://github.com/M42/mikrokosmos.git
cd mikrokosmos
cabal install
```

## First steps

Once installed, you can open the interpreter typing `mikrokosmos` in your terminal. It will show you a prompt where
you can write lambda expressions to evaluate them:

![First steps](https://cloud.githubusercontent.com/assets/5337877/18649151/337c6782-7ebe-11e6-9701-495c2cb40675.gif)

You can write expressions using `\var.` to denote a lambda abstraction on the `var` variable and
you can bind names to expressions using `=`. *But why am I getting this weird output?* Well, the interpreter
outputs the lambda expressions in [De Bruijn notation](https://en.wikipedia.org/wiki/De_Bruijn_notation); it is more
compact and the interpreter works internally with it. However, as you can see in the image, whenever the interpreter finds a known constant, it labels the expression with its name.

If you need help at any moment, you can type `:help` into the prompt to get a summary of the available options:

![Help screen](https://cloud.githubusercontent.com/assets/5337877/18882511/bfc84c34-84df-11e6-8215-870b29e49b8f.gif)

## The standard library

Mikrokosmos comes bundled with a standard library in a file called `std.mkr`; if it was not the case for you, you can download the [library](https://raw.githubusercontent.com/M42/mikrokosmos/master/std.mkr) from the git repository. It allows you to experiment with [Church encoding](https://en.wikipedia.org/wiki/Church_encoding) of booleans,
integers and much more. You can load it with `:load std.mkr`, given the file is in your working directory; after that, you can use a lot of new constants:

![Standard library](https://cloud.githubusercontent.com/assets/5337877/18663278/1a6374e2-7f1e-11e6-99b5-279de7428a10.gif)

All this is written in lambda calculus! You can check the definitions on the `std.mkr` file.

## Debugging and verbose mode

If you want to check how the lambda reductions are being performed you can use the **verbose mode**.
It can be activated and deactivated writing `:verbose`, and it will show you every step on the reduction of
the expression, coloring the substitution at every step.

![Verbose mode](https://cloud.githubusercontent.com/assets/5337877/18882803/060a2dec-84e1-11e6-9dfa-9c08957b559e.gif)

## Advanced data structures

There are representations of structures such as linked lists or trees in the standard library. 
You can use them to do a bit of your usual functional programming:

![Trees](https://cloud.githubusercontent.com/assets/5337877/18883269/d7c3d616-84e2-11e6-9fc9-aa6e3df606f9.gif)

Oh! And you can insert comments with `#`, both in the interpreter and in the files the interpreter can load.

### References & interesting links
* [Build you a Haskell - Stephen Diehl](http://dev.stephendiehl.com/fun/003_lambda_calculus.html)  
* [Haskell from Scratch - Jekor](https://www.youtube.com/playlist?list=PLxj9UAX4Em-Ij4TKwKvo-SLp-Zbv-hB4B)   
* [The Glambda interpreter](https://github.com/goldfirere/glambda)  
* [Lecture notes on the lambda calculus - Peter Selinger](http://www.mscs.dal.ca/~selinger/papers/lambdanotes.pdf)
