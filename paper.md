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

# Description

Mikrokosmos is an untyped and simply-typed lambda calculus
interpreter. 

It aims to provide students with a tool to learn lambda
calculus and intuitionistic logic by coding and to provide educators
with a minimalist and grounded in theory implementation of a
functional programming language.  It can be adopted in Logic and
Computation introductory courses to show how the formal system of the
untyped lambda calculus constitutes a Turing-complete programming
language and how the simply-typed lambda calculus relates to category
theory and intuitionistic logic via the Curry-Howard isomorphism.

Mikrokosmos can be used on three different environments:

 - as a command line executable, implementing a read-eval-print loop
   interpreter;
   
 - as a Jupyter kernel, executing code blocks from a Jupyter notebook
   [@jupyter];
 
 - and as a Javascript web application that can be used in
   conjunction with web text editors such as CodeMirror to create an
   online programming environment.

Many untyped lambda calculus interpreters can be found online. They
usually implement a read-eval-print interpreter that cannot be easily
used with didactic purposes. The following features make Mikrokosmos
different from similar software.

 - A minimalist and unified syntax for simply typed and untyped lambda
   calculus. The same expression will define an untyped term and a
   typed term.  This helps highlighting the differences between the
   two.
   
 - Implicit polymorphism via Curry-style typing. Typed terms adquire
   the most general possible type.  The Curry-Howard correspondence is
   more easily studied on this setting.
   
 - Gentzen style derivation trees. Given any typed expression,
   Mikrokosmos can draw the logical derivation tree it codifies.  In
   particular, it follows most of the notation conventions of
   [@wadler2015propositions].
 
 - Verbose mode with de Bruijn indexes. Mikrokosmos and many programming
   languages based on the lambda calculus use de Bruijn indexes internally.
   Verbose mode shows a step-by-step evaluation of the terms using
   these de Bruijn indexes.
 
 - SKI combinators. Every lambda term can be written in terms of the S
   and K combinators. The interpreter implements this translation,
   allowing the study of this alternative presentation of the lambda
   calculus.
 
 - Standard libraries, with documented functions. The interpreter
   comes bundled with a standard library, and the documentation for
   each of the functions of the library can be consulted directly from
   the interpreter.

To ease a possible future transition to a functional programming
language, Mikrokosmos is heavily inspired by Haskell on its syntax.


# Statement of need

When teaching the logical structure of the lambda-calculus, it is
common to make the students evaluate lambda terms by hand. This
approach seems neccessary at least on the first stages of the learning
proccess, but makes it very tedious to study more complex terms.  Just
as we would never teach the C programming language making students
execute big programs by hand, it makes sense to provide an interpreter
and a programming environment allowing the students to explore lambda
calculus without having to worry about evaluating step-by-step and
focusing on the big picture.

Untyped lambda calculus interpreters are common, but they are usually
meant as programming exercises, and provide ascetic interpreters
lacking any didactic features. Mikrokosmos allows step-by-step
execution, minimalist syntax and multiple ways of showing the results.
It can be integrated directly with preexisting learning modules in the
Jupyter notebook format or as a web page.

The Curry-Howard correspondence (or "Propositions as Types" principle)
between simply-typed lambda calculus and propositional intuitionistic
logic (as described in [@wadler2015propositions] and
[@lambek1988introduction]) is sometimes illustrated using the Haskell
programming language or similar functional programming languages. This
approach, however, is theoretically unsound: Haskell implements a
polymorphic lambda calculus corresponding to second-order
intuitionistic logic; it is not total, making every type inhabited
(this is a fatal flaw for the purpose at hand); and it fails to be the
internal language of a bicartesian closed category due to the support
for non-lazy evaluation. All these deviations from the theory are
justified by the needs of a real-world programming language, but a
student faced with Haskell as an example of the "Propositions as
Types" paradigm may find it difficult to determine exactly what parts
of the language should be ignored.  Mikrokosmos, on the contrary,
faithfully implements the internal language of a bicartesian closed
category without any spurious additions or primitive types.

Simply-typed lambda calculus interpreters of this kind are rare; they
usually implement some primitive types, input-output, and tend not use
Curry typing, making them not as useful to study the Curry-Howard
correspondence.

Mikrokosmos has already been tested on the classroom and the
Mikrokosmos web application can be easily embedded to create
interactive tutorials suited for the needs of each specific course.
In particular, the Mikrokosmos interactive tutorial was used as a
module for the course "Lógica y Programación" (Logic and Programming)
taught by Pedro A. García-Sánchez at the University of Granada. The
students were able to complete the exercises of the tutorial and to
use the interpreter as an aid while completing other problem sets of
the course.

# References


