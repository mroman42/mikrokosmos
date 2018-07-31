---
title: 'Mikrokosmos: an educational lambda calculus interpreter'
tags:
- lambda calculus
- functional programming
- intuitionistic logic
- computability
authors:
- name: Mario Román
  affiliation: 1
affiliations:
- name: University of Granada
  index: 1
bibliography: paper.bib
---

# Description

Mikrokosmos is an educational untyped and simply-typed lambda calculus
interpreter.  For students, it is a tool to experiment with lambda
calculus and intuitionistic logic by coding.  For educators, it is a
educative and grounded in theory implementation of a functional
programming language they can integrate with other learning materials.
Mikrokosmos can be used on three different environments: as a *command
line executable*, implementing a read-eval-print loop interpreter; as
a *Jupyter kernel*, executing code blocks from a Jupyter notebook
[@jupyter]; and as a *Javascript web application*, that can be used in
conjunction with web text editors such as Codemirror [@codemirror] to
create an online programming environment.

Mikrokosmos provides a minimalist and unified syntax for both simply
typed and untyped lambda calculus.  The same expressions can be used
to define untyped and typed terms.  This helps highlighting the
differences between the two and avoids the added complexity of two
different sintaxes.  Moreover, the syntax is heavily inspired by
Haskell so that it facilitates transferring the ideas to a
fully-fledged functional programming.  Mikrokosmos comes also bundled
with a self-documenting standard library of common lambda calculus
combinators and data structures which can be consulted directly from
the interpreter and eases the learning process, demonstrating how to
write basic programs on lambda calculus.

The interpreter focuses on being feature-rich, close to the theory and
suited for learning and experimentation.  For the untyped lambda
calculus, Mikrokosmos works as a Turing-complete programming
language. Following [@barendregt84], it implements the *leftmost
evaluation strategy*, which finds a normal form whenever it exists,
allowing infinite data structures and fixed point definitions.  The
evaluation can be visualized step-by-step, showing how the internal
representation of a lambda term using de Bruijn indexes
[@de1972lambda] works.  Optional coloring of the output can facilitate
this purpose.  With regards to combinatorial logic, a translation
between lambda terms and SKI combinators is available. It follows a
standard algorithm [@hindley2008lambda] that allows the user to input
and visualize results in both formats.  On the typed side, Mikrokosmos
facilitates the study of the Curry-Howard isomorphism
[@sorensen2006lectures].  To this end, it implements implicit typing à
la Curry: the most general possible type is automatically inferred for
any term, providing a limited form of polymorphism.  Given any typed
lambda expression, Mikrokosmos can visualize the logical derivation
tree it codifies in the style of Gentzen, sharing the notation of
introductory articles and textbooks [@wadler2015propositions];
[@girard1989proofs].

![](fig1.png)
*Figure 1. An example Gentzen diagram of a proof on intuitionistic logic.*

The latest release of the interpreter can be installed via `stack` or
`cabal` and the documentation can be found at
[https://mroman42.github.io/mikrokosmos/], where usage instructions
for the Jupyter kernel and Javascript web application can be found.


# Statement of need

When teaching the logical structure of the lambda-calculus, it is
common to make the students evaluate lambda terms by hand. This
approach seems neccessary at least on the first stages of the learning
proccess, but makes it very tedious to study more complex terms.  Just
as we would never teach the C programming language making students
execute big programs by hand, it makes sense to provide an interpreter
and a programming environment allowing the students to explore lambda
calculus without having to worry about evaluating step-by-step and
focusing on the big picture. There is a need for an lambda calculus
interpreter. Untyped lambda calculus interpreters are common, but they
are usually meant as programming exercises, and provide ascetic
interpreters lacking any didactic features. Mikrokosmos addresses the
need for an educational interpreter with multiple ways of visualizing
the results, a clean and self-coherent syntax, libraries, and the
possibility of integration in web pages and Jupyter notebooks.

The Curry-Howard isomorphism between simply-typed lambda calculus and
propositional intuitionistic logic [@wadler2015propositions] is
sometimes illustrated using the Haskell programming language or
similar functional programming languages. This approach, however, is
theoretically questionable: Haskell implements a polymorphic lambda
calculus [@reynolds1994introduction] corresponding to second-order
intuitionistic logic [@wadler2007girard]; but it is not total, making
every type inhabited (which is a fatal flaw for the purpose at hand);
and it fails to represent a cartesian closed category mainly due to
its support for strict evaluation, partial functions and undefined
values [@danielsson2006fast]. All these deviations from the theory are
justified by the needs of a real-world programming language, but a
student faced with Haskell as an example of the "Propositions are
Types" paradigm may find it difficult to determine exactly what parts
of the language should be ignored in order to retain a sound
interpretation.  Mikrokosmos, on the contrary, simply implements
the internal language of a bicartesian closed category
[@lambek1988introduction] without any spurious additions nor primitive
types.

Simply-typed lambda calculus interpreters of this kind are rare; the
existing software usually implements some primitive types,
input-output, and tend not use Curry typing, making them not as clear
to study the Curry-Howard correspondence.

Mikrokosmos has already been tested on the classroom and the
Mikrokosmos web application can be easily deployed to create
interactive tutorials suited for the needs of each specific course.
This software could be particularly useful in logic or computation
introductory courses to show how the formal system of the untyped
lambda calculus constitutes a Turing-complete programming language and
the Curry-Howard isomorphism.  In particular, Mikrokosmos and its
interactive tutorial have already been used as a module for the course
"Lógica y Programación" (Logic and Programming) taught by Pedro
A. García-Sánchez at the University of Granada. The students were able
to complete the exercises of the tutorial and to use the interpreter
as an aid while completing other problem sets of the course.

# Acknowledgements

# References



