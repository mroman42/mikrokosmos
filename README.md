# mikrokosmos

**Mikrokosmos** is an untyped lambda calculus interpreter, borrowing its name from the series of
progressive piano études *[Mikrokosmos](https://www.youtube.com/watch?v=VEsMk3DAzWM)* written by *Bela Bartok*. 
It aims to provide students with a tool to learn and understand lambda calculus.
If you want to start learning about lambda calculus, I suggest you to read:

 * [The wikipedia page on Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus#Informal_description)
 * [A tutorial introduction to the Lambda calculus by Raúl Rojas](www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf)

And to install and to tinker with this interpreter.

## Installation

Mikrokosmos will be soon installable from [Hackage](http://hackage.haskell.org/). Meanwhile, you can install it 
cloning the repository and using [cabal](https://www.haskell.org/cabal/):

``` bash
git clone https://github.com/M42/mikrokosmos.git
cd mikrokosmos
cabal install
```

## First steps

Once installed, you can open the interpreter typing `mikrokosmos` in your terminal. It will show you a prompt where
you can write lambda expressions to evaluate them:

![First steps](https://cloud.githubusercontent.com/assets/5337877/18393670/92728f10-76b6-11e6-88cc-88e7f2cb9114.png)

You can write expressions using `\var.` to denote a lambda abstraction on the `var` variable and
you can bind names to expressions using `=`. *But why are you getting this weird output?* Well, the interpreter
outputs the lambda expressions in [De Bruijn notation](https://en.wikipedia.org/wiki/De_Bruijn_notation); it is more
compact and the interpreter works internally with it. However, as you can see in the image, whenever the interpreter finds a known constant, it labels the expression with its name.

If you need help at any moment, you can type `:help` into the prompt to get a summary of the available options:

![Help screen](https://cloud.githubusercontent.com/assets/5337877/18393812/33e86b6c-76b7-11e6-818b-76b68f599a44.png)

## The standard library

Mikrokosmos comes bundled with a standard library. It allows you to experiment with Church encoding of booleans,
integers and much more. You can load it with `:load std.mkr`; after that, you can use a lot of new constants:

![Standard library](https://cloud.githubusercontent.com/assets/5337877/18394001/1a238ec2-76b8-11e6-90ad-b2385ba60268.png)

All this is written in lambda calculus! You can check the definitions on the `std.mkr` file.

## Debugging and verbose mode

If you want to check how the lambda reductions are being performed you can use the **verbose mode**.
It can be activated and deactivated writing `:verbose`, and it will show you every step on the reduction of
the expression, coloring the substitution at every step.

![Verbose mode](https://cloud.githubusercontent.com/assets/5337877/18394177/e4925bd4-76b8-11e6-886e-6bd33fe02e88.png)

## Advanced data structures

There are representations of structures such as linked lists or trees in the standard library. 
You can use them to do a bit of your usual functional programming:

![Trees](https://cloud.githubusercontent.com/assets/5337877/18394894/5cd41d1e-76bc-11e6-9564-8817992392af.png)

Oh! And you can insert comments with `#`, both in the interpreter and in the files the interpreter can load.

### References & interesting links
* [Build you a Haskell - Stephen Diehl](http://dev.stephendiehl.com/fun/003_lambda_calculus.html)  
* [Haskell from Scratch - Jekor](https://www.youtube.com/playlist?list=PLxj9UAX4Em-Ij4TKwKvo-SLp-Zbv-hB4B)   
* [The Glambda interpreter](https://github.com/goldfirere/glambda)  
* [Lecture notes on the lambda calculus - Peter Selinger](http://www.mscs.dal.ca/~selinger/papers/lambdanotes.pdf)
