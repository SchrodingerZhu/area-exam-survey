#import "@preview/touying:0.4.0": *
#import "@preview/curryst:0.2.0" as curryst: rule, proof-tree
#import "@preview/fletcher:0.4.2" as fletcher: node, edge

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
#(s.methods.touying-new-section-slide = none)
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

#pagebreak()

#text(size: 12pt)[
```
[4399/4429] Building Mathlib.Probability.Independence.ZeroOne
[4403/4429] Building Mathlib.Analysis.SpecialFunctions.Gamma.Deligne
[4404/4429] Building Mathlib.Analysis.SpecialFunctions.Gaussian.PoissonSummation
[4404/4429] Building Mathlib.Analysis.Fourier.Inversion
[4406/4429] Building Mathlib.NumberTheory.NumberField.CanonicalEmbedding.ConvexBody
[4407/4429] Building Mathlib.NumberTheory.ModularForms.JacobiTheta.TwoVariable
[4409/4429] Building Mathlib.NumberTheory.LSeries.MellinEqDirichlet
[4410/4429] Building Mathlib.Analysis.MellinInversion
[4413/4429] Building Mathlib.NumberTheory.ModularForms.JacobiTheta.Bounds
[4413/4429] Building Mathlib.NumberTheory.ModularForms.JacobiTheta.OneVariable
[4414/4429] Building Mathlib.NumberTheory.ModularForms.JacobiTheta.Manifold
[4414/4429] Building Mathlib.NumberTheory.ZetaFunction
[4416/4429] Building Mathlib.NumberTheory.LSeries.HurwitzZetaEven
[4418/4429] Building Mathlib.NumberTheory.NumberField.Discriminant
[4418/4429] Building Mathlib.NumberTheory.NumberField.Units
[4419/4429] Building Mathlib.NumberTheory.LSeries.Dirichlet
[4419/4429] Building Mathlib.NumberTheory.EulerProduct.DirichletLSeries
[4419/4429] Building Mathlib.NumberTheory.Harmonic.ZetaAsymp
[4424/4429] Building Mathlib.NumberTheory.Cyclotomic.Discriminant
[4424/4429] Building Mathlib.NumberTheory.NumberField.ClassNumber
[4425/4429] Building Mathlib.NumberTheory.Cyclotomic.Rat
[4427/4429] Building Mathlib.NumberTheory.Cyclotomic.PID
[4428/4429] Building Mathlib
18895.18user 900.24system 18:24.06elapsed 1792%CPU (0avgtext+0avgdata 2392648maxresident)k
222728347inputs+6915077outputs (446465major+109070444minor)pagefaults 0swaps
```
]

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

- Elimination Rule (Non-Dependent Version)
$
elimination
$

#pagebreak()

- Computation Rules
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

- Computation of function programming are associated with introductions and eliminations.

- These rules are closely related to pattern matchings and core language (IR) transformations.

- Efficiently handling of introductions and eliminations is a key factor to performance.

= Reuse Analysis

== The Essence of Memory Reuse

- Introductions and Eliminations involve memory allocation and memory deallocations.

- Global allocators try hard to hide latency but they can still be heavy due to atomicity.

- Can we minimize the impact due to memory allocations?

== RC-based Memory Management

#figure(image("images/mimalloc.png", width: 50%), caption: "Mimalloc Heap Layout")

- Modern allocators use sharded freelists.
- If memory resource is released in time, it becomes ready to be reused.

== RC-based Memory Management
Let's do it!

```hs
reverse Nil        acc = acc
reverse (Cons h t) acc = reverse t (Cons h acc)
```

#pause
Translate to RC-based runtime:

```rs
pub fn reverse(xs: Rc<List>, acc: Rc<List>) -> Rc<List> {
  match *xs {
    List::Nil => acc,
    List::Cons(ref x, ref xs) => 
      reverse(xs.clone(), Rc::new(List::Cons(x.clone(), acc)))
  }
}
```

== RC-based Memory Management

Let's insert drop once an object is no longer being used:

```rs
pub fn reverse(xs: Rc<List>, acc: Rc<List>) -> Rc<List> {
  match *xs {
    List::Cons(ref y, ref ys) => {
      let y = y.clone();
      let ys = ys.clone();
      drop(xs);
      reverse(ys, Rc::new(List::Cons(y, acc)))
    }
    List::Nil => { omitted!() },
  }
}
```

== Drop Specialization

- We have "cold" drop routines on every execution.
- `y`, `ys` are cloned but they may be immediately dropped inside `drop(xs)`. 
- What if we inline the destruction routine?

#pagebreak()

```rs
pub fn reverse(xs: Rc<List>, acc: Rc<List>) -> Rc<List> {
  match xs {
    List::Nil => acc,
    List::Cons(ref y, ref ys) => {
      let y = y.clone();
      let ys = ys.clone();
      let mem = if Rc::is_unique(xs) {
        drop(y);
        drop(ys);
        Some(Rc::take_memory(xs))
      } else { None };
      reverse(ys, Rc::reuse_or_alloc(mem, List::Cons(y, acc)))
    }
  }
}
```

#pagebreak()
*Push Down*:
#text(size: 18pt)[
```rs
pub fn reverse(xs: Rc<List>, acc: Rc<List>) -> Rc<List> {
  match xs {
    List::Nil => acc,
    List::Cons(ref y, ref ys) => {
      let mem = if Rc::is_unique(xs) {
        Some(Rc::take_memory(xs)) // hot path
      } else { 
        let y = y.clone();
        let ys = ys.clone();
        None 
      };
      reverse(ys, Rc::reuse_or_alloc(mem, List::Cons(y, acc)))
    }
  }
}
```
]
== Frame-Limited Reuse

Decides reuse opportunity can be challenging with nested structures. Objects can be conditionally referenced.

```rs
pub fn foo(bar: Rc<List>, baz: bool) -> Rc<List> {
  match bar {
    List::Cons(hd, tl) => {
      match baz {
        true => bar, // bar is not possible for reuse
        false => List::Cons(1, tl), // bar is possible for reuse
      }
    }
    _ => omitted!()
  }
}
```

#pagebreak()

Optimistically insert reuse for current frame can be harmful:

```rs
pub fn foo(bar: Rc<List>, baz: bool) -> Rc<List> {
  match bar {
    List::Cons(_, _) => {
      let qux = quux(bar)
      Rc::new(Cons(qux, Nil))
      // let qux = quux(bar.clone())
      // let token = drop_with_memory(bar);
      // Rc::reuse_or_alloc(token, Cons(qux, Nil))
    }
    _ => omitted!()
  }
}
```
#pagebreak()
*Drop-Guided Reuse Analysis*
- First decide last-use frontiers
- Insert drop that creates a reuse token in such context
- Going down the paths to find possible reuse
- Clean up unused tokens

= High-level Memory Reuse Runtime

== Overview of the IR

#text(size: 12pt)[
```rust
module test {
    enum List<T>
        where @T: Foo
    { Nil, Cons(@T, List<@T>) }
    fn test<T>(%1: i32, %2: f64) -> i32 
      where @T: std::TraitFoo + std::TraitBar<Head = ()> 
    {
        %3 = constant 3 : i32;
        return %3;
    }
    fn test2<T : Ord>(%0 : @T, %1 : @T) -> List<@T> {
        %2 = %0 < %1;
        %3 = new List::Nil;
        if %2 {
          %4 = new List::Cons, %0, %3
          return %4;
        } else {
          %5 = new List::Cons, %0, %1;
          return %5;
        }
    }
}
```
]

== Problems to solve

We want to compile RC-based memory reused languages to a high-level host language (Rust).

- Get rid of type erasure and boxing.
- Interpolate between host and guest languages directly.
- Use HOAS to efficiently compile closures.
  - Keep type info.
  - Make it comprehensible for host language.
- Make the code more friendly modern compiler optimizations.
  - Explicit uniqueness in type system.
- Make the IR type polymorphic.
  - How to decide reuse? (layout alpha-equivalence)
- Support fusion.



=== No type erasure and boxing
Build up a morphism between IR types and Rust's type.
$
"Managed" &= {"Primitive", "Rc<X>"}  \
"Primitive" &arrow.bar "Primitive" \
"TypeVar" &arrow.bar "TypeVar" : "Managed" \
"Object" &arrow.bar "Rc<Object>"
$
A by-product: Rust can check the correctness of codegen.

=== Seamless Interpolation

- Manipulation RC in Rust is not handy (e.g. no nested pattern matching)
- We can make the guest language as a DSL for RC.
- But, can we pass objects directly across language boundaries? 

```rust
// In Rust
extern "rust" fn Vec::push<T>(mut v : Rc<Vec<T>>, t: T) : Rc<Vec<T>> {
  v.make_mut().push(t);
  v
}
// In an imaginary functional language
collect : u32 -> Vec<u32> -> Vec<u32>
collect 0 acc = acc
collect x acc = collect (x - 1) (Vec::push acc x)
```

#pagebreak()

Actually, yes!

```rust 
impl<T : Clone + ?Sized> Rc<T> {
  fn make_mut(&mut self) -> &mut T {
    if !Rc::is_unique(self) {
      // clone if the Rc pointer is not exclusive
      *self = Rc::new(self.clone());
    } 
    // obtain a mutable referece in place
    unsafe { &mut *(&*self as *const _ as *mut _)  }
  }
}
```

=== Efficient HOAS

First-class closure is a nightmare. Partial application worsen the situation.

```c
typedef struct {
    lean_object   m_header;
    void *        m_fun;
    uint16_t      m_arity;     
    uint16_t      m_num_fixed;
    lean_object * m_objs[0];
} lean_closure_object;
```
- Function is erased to `void *`.
- Closure passing back from guest language is "opaque".
- Needs a uniform ABI (boxing all arguments), duplicating the code. 

#pagebreak()

It is possible to play with Rust's type system to encode the closure.
```rs
pub struct Thunk<F, P: PartialParams, R>
where
    F: FnOnce(P::Full) -> R + Clone,
{
    code: F,
    params: P,
}
// Use Ready and Hole to Tag application status
impl<T0: Clone, T1: Clone, T2: Clone, T3: Clone> PartialParams
for (Ready<T0>, Ready<T1>, Ready<T2>, Hole<T3>) {
    type Pending = (T3,);
    type Progress = (Ready<T0>, Ready<T1>, Ready<T2>, Ready<T3>);
}
```

#pagebreak()

```rust
module test {
  fn test() -> fn (i32, i32) -> i32 {
    %0 = constant 991208 : i32;
    %1 = (%2 : i32, %3 : i32) -> i32 {
      %4 = %2 + %3;
      %5 = %4 + %0;
      return %5;
    };
    return %1;
  }
}
```

=== Uniqueness Type System without Isolation

*A Dilemma*
1. We want explicit uniqueness type to remove unwanted checks (to make SIMD applicable).
2. We don't want explicit uniqueness type as it forbids us passing objects around.

#let ref-diagram = { 
  let Rc = (0, -1)
  let Unique = (0, 1)
  let Ref = (1, 0)
  let Token = (-1, 0)
  fletcher.diagram(
    node-stroke: 1pt,
    edge-stroke: 1pt,
    node(Rc, "Rc<T>"),
    node(Unique, "Unique<T>"),
    node(Ref, "&T"),
    node(Token, "Token<T>"),
    edge(Unique, Rc, "<|-", [uniquefy], label-pos: 0.9),
    edge(Unique, Rc, "-|>", [direct cast], label-pos: 0.1),
    edge(Rc, Ref, "-|>", bend: 30deg, [borrow]),
    edge(Unique, Ref, "-|>", bend: -30deg, [borrow]),
    edge(Token, Rc, "-|>", bend: 30deg, [reuse], label-pos: 0.1),
    edge(Rc, Token, "-|>", bend: 30deg, [drop], label-pos: 0.7),
    edge(Token, Unique, "-|>", bend: -30deg, [reuse], label-pos: 0.1),
    edge(Unique, Token, "-|>", bend: -30deg, [drop], label-pos: 0.7),
  )
}
#text(size: 17pt)[
#figure(
  ref-diagram,
  caption: [Reference Sorts])]

=== Open Type Variables

#let basic-eq = curryst.rule(
  label: [$PP_equiv$],
  [$P_0 equiv P_1$],
  [$ell(P_0) = ell(P_1)$],
)
#let basic-neq = curryst.rule(
  label: [$PP_equiv.not$],
  [$P_0 equiv.not P_1$],
  [$ell(P_0) eq.not ell(P_1)$],
)
#let var-eq = curryst.rule(
  label: [$VV_equiv$],
  [$V_0 equiv V_0$],
)
#let var-sim = curryst.rule(
  label: [$VV_tilde.equiv$],
  [$V_0 tilde.equiv V_1$],
)
#let sum-equiv = curryst.rule(
  label: [$plus_equiv$],
  [$A_0 + B_0 equiv A_1 + B_1$],
  [$A_0 equiv A_1$],
  [$B_0 equiv B_1$],
)
#let sum-ne-l = curryst.rule(
  label: [$plus_(equiv.not,L)$],
  [$A_0 + B_0 equiv.not A_1 + B_1$],
  [$A_0 equiv.not A_1$],
)
#let sum-ne-r = curryst.rule(
  label: [$plus_(equiv.not,R)$],
  [$A_0 + B_0 equiv.not A_1 + B_1$],
  [$B_0 equiv.not B_1$],
)
#let sum-sim-l = curryst.rule(
  label: [$plus_(tilde.equiv,L)$],
  [$A_0 + B_0 tilde.equiv A_1 + B_1$],
  [$A_0 tilde.equiv A_1$],
  [$B_0 equiv B_1$],
)
#let sum-sim-r = curryst.rule(
  label: [$plus_(tilde.equiv,R)$],
  [$A_0 + B_0 tilde.equiv A_1 + B_1$],
  [$A_0 equiv A_1$],
  [$B_0 tilde.equiv B_1$],
)
#let sum-sim = curryst.rule(
  label: [$plus_(tilde.equiv)$],
  [$A_0 + B_0 tilde.equiv A_1 + B_1$],
  [$A_0 tilde.equiv A_1$],
  [$B_0 tilde.equiv B_1$],
)
#let prod-equiv = curryst.rule(
  label: [$times_equiv$],
  [$A_0 times B_0 equiv A_1 times B_1$],
  [$A_0 equiv A_1$],
  [$B_0 equiv B_1$],
)
#let prod-ne-l = curryst.rule(
  label: [$times_(equiv.not,L)$],
  [$A_0 times B_0 equiv.not A_1 times B_1$],
  [$A_0 equiv.not A_1$],
)
#let prod-ne-r = curryst.rule(
  label: [$times_(equiv.not,R)$],
  [$A_0 times B_0 equiv.not A_1 times B_1$],
  [$B_0 equiv.not B_1$],
)
#let prod-sim-l = curryst.rule(
  label: [$times_(tilde.equiv,L)$],
  [$A_0 times B_0 tilde.equiv A_1 times B_1$],
  [$A_0 tilde.equiv A_1$],
  [$B_0 equiv B_1$],
)
#let prod-sim-r = curryst.rule(
  label: [$times_(tilde.equiv,R)$],
  [$A_0 times B_0 tilde.equiv A_1 times B_1$],
  [$A_0 equiv A_1$],
  [$B_0 tilde.equiv B_1$],
)
#let prod-sim = curryst.rule(
  label: [$times_(tilde.equiv)$],
  [$A_0 times B_0 tilde.equiv A_1 times B_1$],
  [$A_0 tilde.equiv A_1$],
  [$B_0 tilde.equiv B_1$],
)
#let inference-rules = {
let pt = curryst.proof-tree;
$
#pt(basic-eq)   #pt(basic-neq)  #pt(var-sim) #pt(var-eq)  \
#pt(sum-equiv)  #pt(sum-ne-l)   #pt(sum-ne-r)  \ 
#pt(sum-sim-l)  #pt(sum-sim-r)  #pt(sum-sim)   \
#pt(prod-equiv) #pt(prod-ne-l)  #pt(prod-ne-r) \ 
#pt(prod-sim-l) #pt(prod-sim-r) #pt(prod-sim)
$
}

$
#inference-rules
$

=== Specialization

```rs
List::Cons(ref y, ref ys) => {
  let y = y.clone();
  let ys = ys.clone();
  let mem = if Rc::is_unique(xs) {
    drop(y);
    drop(ys);
    Some(Rc::take_memory(xs))
  } else { None };
  reverse(ys, Rc::reuse_or_alloc(mem, List::Cons(y, acc)))
}
```

#pagebreak()

```rust
List::Cons(..) => {
  let (token, List::Cons(y, ys)) 
    = Rc::unwrap_for_reuse(xs) else { core::hint::unreachable() };
  reverse(ys, Rc::reuse_or_alloc(mem, List::Cons(y, acc)))
}
```


= Open Problems

- *Dataflow Formulation*
 - Support imperative programming
- *Optimal Memory Reuse*
 - Decide paring under frame limitation
 - Profiling
- *Value Sharing Problem*
- *GC Integration*

== Value Sharing Problem

```js 
fun subst(t : tree<a>, x : int, y: int) : tree<a>
  match t 
    Node(l, v, r) ->
      if v == x then Node(subst(l, x, y), y, subst(l, x, y))
                else Node(subst(l, x, y), v, subst(l, x, y))
    Leaf -> t
```

- `if Rc::eq(input, output) {return original}`
- Interfere with reuse analysis.
- Tail recursion can be problematic.

== GC Integration

- How to handle cycles?
- How to handle concurrent RC?

#figure(image("images/gc.png", width: 35%))

= Conclusion

*Conclusion*

- Why Functional Programming Demands Memory Reuse
- Reuse Analysis (Inplace Updates)
- Challenges of Targeting High-level Languages
- Open Problems




