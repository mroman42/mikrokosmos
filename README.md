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
you can write lambda expressions to be evaluated.



You can write expressions using `\var.` to denote a lambda abstraction on the `var` variable and
you can bind names to expressions using `=`. *But why are you getting this weird output?* Well, the interpreter
outputs the lambda expressions in [De Bruijn notation](https://en.wikipedia.org/wiki/De_Bruijn_notation); it is more
compact and the interpreter works internally with it.

### References & interesting links
* [Build you a Haskell - Stephen Diehl](http://dev.stephendiehl.com/fun/003_lambda_calculus.html)  
* [Haskell from Scratch - Jekor](https://www.youtube.com/playlist?list=PLxj9UAX4Em-Ij4TKwKvo-SLp-Zbv-hB4B)   
* [The Glambda interpreter](https://github.com/goldfirere/glambda)  
* [Lecture notes on the lambda calculus - Peter Selinger](http://www.mscs.dal.ca/~selinger/papers/lambdanotes.pdf)
