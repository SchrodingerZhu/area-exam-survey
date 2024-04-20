#import "@preview/touying:0.4.0": *
#import "@preview/curryst:0.2.0" as curryst: rule, proof-tree

#let s = themes.dewdrop.register(
  aspect-ratio: "16-9",
  footer: [Area Exam],
  navigation: "mini-slides",
  // navigation: "sidebar",
  // navigation: none,
)
#let s = (s.methods.info)(
  self: s,
  title: [$R^3$ : Reuse Analysis, Review and
Rethinking],
  subtitle: [Area Exam],
  author: [Yifan Zhu],
  date: datetime.today(),
  institution: [University of Rochester],
  outline-slide: false
)
#let (init, slides, touying-outline, alert) = utils.methods(s)
#show: init

#show strong: alert

#let (slide, empty-slide, title-slide, new-section-slide, focus-slide) = utils.slides(s)


#show: slides.with(outline-slide: false)

= Before We Start
== The Development of Proof Assistants
It soon became clear that the only real long-term solution to the problems that I encountered is to start using computers in the verification of mathematical reasoning. (Vladimir Voevodsky)
#figure(image("images/voevodsky.png", width: 20%))

== The Development of Proof Assistants
#box(
  columns(3)[
    #set par(justify: true)
    #v(1cm)
  #figure(image("images/coq.png", width: 60%))
  #colbreak()
  #v(4cm)
  #figure(image("images/agda.png", width: 100%))
  #colbreak()
  #v(3cm)
  #figure(image("images/lean_logo.svg", width: 100%))
  ]
)

== The Development of Proof Assistants

#link("https://hirrolot.github.io/posts/compiler-development-rust-or-ocaml.html")[Compiler Development: Rust or OCaml?]

Many language researchers favor functional programming:

1. Friendly syntax for implementing complicated operations (CPS transformation takes 2x lines to write in Rust)
2. GADT/Functor abstractions (First-class modules)
4. (Addtionally) Good garbage collection for nontrivial memory patterns

= Functional Programming Patterns

== Inductive Types

- Primitive Types
  - Int
  - Chat/String
  - Unit 
- Inductive Construction
  ```lean
  inductive List (A : Type)
    | Nil
    | Cons A List
  ```
== Inductive Types


#let rule = curryst.rule(
  label: [],
  [$"List" A "type"$],
  [$A "type"$],
)
#let tree = curryst.proof-tree(rule)

- Type Formation Rule
$
tree
$


