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
  - Char/String
  - Unit 
- Inductive Construction (Sum/Product)
  ```lean
  inductive List (A : Type)
    | Nil
    | Cons A (List A)
  ```

#pagebreak()


#let formation = curryst.proof-tree(curryst.rule(
  label: [],
  [$"List" A "type"$],
  [$A "type"$],
))

- Type Formation Rule
$
formation
$

#let intro1 = curryst.proof-tree(curryst.rule(
  label: [],
  [$"Nil : List A"$],
  [$" "$],
))

#let intro2 = curryst.proof-tree(curryst.rule(
  label: [],
  [$"Cons"(x, y) : "List" A$],
  [$x : A$],
  [$y : "List" A$]
))

- Introduction Rules
$
intro1
intro2
$

#let elimination = curryst.proof-tree(curryst.rule(
  label: [],
  [$"Elim"_"List A" (ell, square_"Nil", f_"Cons") : B$ ],
  [$ell  : A$],
  [$square_"Nil" : star ->B $],
  [$f_"Cons" : A times "List" A -> B$]
))

- Elimination Rule (Non-Dependent Version, $beta$-rule)
$
elimination
$

#pagebreak()

- Conversion Rules ($eta$-rules)

#let conv1 = curryst.proof-tree(curryst.rule(
  label: [],
  [$"Elim"_"List A" ("Nil", square_"Nil", f_"Cons") equiv  square_"Nil" (star) : B$ ],
  [$square_"Nil" : star ->B $],
  [$f_"Cons" : A times "List" A -> B$]
))

#let conv2 = curryst.proof-tree(curryst.rule(
  label: [],
  [$"Elim"_"List A" ("Cons"(x, y), square_"Nil", f_"Cons") equiv  f_"Cons" (x, y) : B$ ],
  [$square_"Nil" : star -> B $],
  [$f_"Cons" : A times "List" A -> B$],
  [$x : A$],
  [$y : "List" A$],
))

$
conv1 \
conv2
$

== Computation as Introductions and Eliminations
=== Redblack Tree 

#text(size: 19pt)[
```haskell
balance B (Node R (Node R a x b) y c) z d = Node R (Node B a x b) y (Node B c z d)
balance B (Node R a x (Node R b y c)) z d = Node R (Node B a x b) y (Node B c z d)
balance B a x (Node R b y (Node R c z d)) = Node R (Node B a x b) y (Node B c z d)
balance B a x (Node R (Node R b y c) z d) = Node R (Node B a x b) y (Node B c z d)
balance color l k r = Node color l k r
```
]

=== NbPE

$
TT &: "Term Domain"\
VV &: "Value Domain" \
lr(bracket.l.double dot.c bracket.r.double) &: TT -> VV \
"eval" &: VV -> VV \
"reify" &: VV -> TT \
"NbPE" &: TT -> TT \
"NbPE" &= "reify" compose "eval" compose lr(bracket.l.double dot.c bracket.r.double) 
$

#pagebreak()

```hs   
eval :: Env -> Tm -> Val
eval env = \case
  Var x     -> fromJust $ lookup x env
  App t u   -> eval env t $$ eval env u
  Lam x t   -> VLam x (\u -> eval ((x, u):env) t)
  Let x t u -> eval ((x, eval env t):env) u

quote :: [Name] -> Val -> Tm
quote ns = \case
  VVar x                 -> Var x
  VApp t u               -> App (quote ns t) (quote ns u)
  VLam (fresh ns -> x) t -> Lam x (quote (x:ns) (t (VVar x)))
```

== Takeaways

- Introductions, Eliminations and Eta-conversions encode the computation of function programming.

- These rules are closely related to pattern matchings and core language (IR) transformations.

- Efficiently handling of introductions and eliminations is a key factor to performance.

= Reuse Analysis