---
title: 'Mikrokosmos: a didactic lambda calculus interpreter'
tags:
- lambda calculus
- functional programming
- intuitionistic logic
authors:
- name: Mario Román
  affiliation: 1
affiliations:
- name: University of Granada
  index: 1
bibliography: paper.bib
---

    (around 1000 words, for a non-specialist)

# Description

Mikrokosmos is an untyped and simply-typed lambda calculus
interpreter.  It aims to provide students with a tool to learn lambda
calculus by coding and to provide educators with an minimalist and
grounded in theory implementation of a functional programming
language.  In particular, it can be used in Logic and Computation
introductory courses to show how the formal system of the untyped
lambda calculus constitutes a Turing-complete programming language and
how the simply-typed lambda calculus relates to category theory and
intuitionistic logic via the Curry-Howard isomorphism.

Mikrokosmos can be used on three different environments.

 - As a command line executable, implementing a read-eval-print loop
   interpreter.
   
 - As a Jupyter kernel, executing code blocks from a Jupyter notebook.
 
 - As a Javascript web application, that can be used in conjunction
   with web text editors such as CodeMirror to create a programming
   web environment.

Features TODO

 - Unified syntax for simply typed and untyped lambda calculus.
 - Gentzen style derivation trees.
 - Verbose mode, de Bruijn indexes. How it works internally.
 - Lazy and infinite data structures.
 - SKI combinators.
 - Optional coloring of the output.
 - Standard libraries, with documented functions.

# Statement of need

Although many untyped lambda calculus interpreters can be found
online, they usually implement a simple interpreter that cannot be
easily used to direct a course on lambda calculus, usually lacking the
majority of the features described above.

The Curry-Howard-Lambek correspondence between simply-typed lambda
calculus, propositional intuitionistic logic and cartesian closed
categories (as described in [@wadler2015propositions] and
[@lambek1988introduction]) is usually illustrated through the Haskell
programming language. This approach, however, is theoretically
unsound: Haskell implements a polymorphic lambda calculus
corresponding to second-order intuitionistic logic; and the language
fails to be the internal language of a cartesian closed category
mainly because of the support for non-lazy evaluation and
input-output.  The student faced with Haskell as an example the
Curry-Howard-Lambek correspondence may find it difficult to determine
exactly what parts of the language should they be ignoring.
Mikrokosmos on the contrary, faithfully implements the internal
language of a bicartesian closed category without any spurious
additions or primitive types.

To ease a possible future transition to a functional programming
language, Mikrokosmos is heavily inspired by Haskell on its syntax.
Simply-typed lambda calculus interpreters are in general more rare;
the glambda interpreter for instance is also based on Haskell, but it
lacks the multienvironment support and many of the features
Mikrokosmos offers.

Mikrokosmos has already been tested on the classroom and the
Mikrokosmos web application can be easily embedded to create
interactive tutorials suited for the needs of each specific course.
In particular, the Mikrokosmos interactive tutorial was used as a
module for the course "Lógica y Programación" (Logic and Programming)
taught by Pedro A. García-Sánchez at the University of Granada. The
students were able to complete the exercises of the tutorial and to
use the interpreter as a help while completing other problem sets of
the course.

# References

