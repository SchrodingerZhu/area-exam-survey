#import "template.typ": *
#import "common/titlepage.typ": *
#import "typst/openning.typ": *
#import "typst/disclaimer.typ": *
#import "typst/acknowledgement.typ": *
#import "typst/abstract_en.typ": *
#import "common/metadata.typ": *
#import "@preview/fletcher:0.4.2" as fletcher: node, edge
#import "@preview/curryst:0.2.0" as curryst: rule, proof-tree
#import "reuse.typ": reuse-section
#titlepage(
  title: titleEnglish,
  advisor: advisor,
  author: author,
  submissionDate: submissionDate
)
#openning(
  title: titleEnglish,
)
#disclaimer(
  title: titleEnglish,
  author: author,
  submissionDate: submissionDate
)

// #acknowledgement()

#abstract_en()

#show: project.with(
  title: titleEnglish,
  advisor: advisor,
  author: author,
  submissionDate: submissionDate
)

#set text(font: "EB Garamond")

= Introduction
In recent years, proof assistants have surged in popularity across both academic and industrial domains. Computer scientists begin to use proof assistants to verify realistic software systems as exemplified by @Leroy-Compcert-CACM @composable-verification. Simultaneously, mathematicians are beginning to seriously integrate computer-aided proofs into their workflows @terence_tao_lean_tour @Fermat_last_theorem. Such trend leads to serious considerations for the design and implementation of high-performance functional programming languages.

An emerging challenge lies in efficiently managing memory resources for functional languages. Essentially, the memory management policy must address rapid allocations and deallocations to enhance memory reuse and locality. In a general context, global allocators strive to adapt to these patterns by establishing various levels of free lists, as discussed in @locality-alloc @leijen2019mimalloc @snmalloc. Functional programming, distinguished by its inherent immutability, requires even more frequent memory reuse to accommodate its specific needs.

The memory resources of functional languages are typically managed by garbage collectors. Depending on different user scenarios, various implementations may configure their collection algorithms with distinct characteristics, as evidenced by the diverse approaches in @haskell, @ocaml, @ocaml-pm, @erlang-1, and @erlang-2. Despite these differences, these advanced collection algorithms universally adopt generational approaches, underscoring the significance of locality and efficient memory reuse in frequently accessed (hot) regions. Recently developed languages like Koka and Lean 4 have illuminated the potential of combining RC-based (Reference Counting) runtime and reuse analysis to achieve in-place mutability within a purely functional environment, as indicated by @lean-4 @perceus @frame-limited @fp2. Researchers have termed this innovative approach as the "functional-but-in-place" (FBIP) paradigm. 

This work examines the characteristics of functional programming alongside previous research in the domain of memory management and reuse analysis. By conducting case studies, this study aims to provide a comprehensive understanding of the essence of memory reuse, as well as the advantages and disadvantages of the new RC-based methods. Furthermore, this work will suggest potential enhancements to address existing challenges in reuse analysis and the RC-based runtime framework.

= Functional Programming
== A Brief Overview
Functional programming is typically associated with a paradigm that formulates programs as lambda expressions and views computation as the β-reduction or normalization of lambda terms. In a purely functional framework, evaluations are devoid of side effects, allowing programs to be regarded as "functions" in the mathematical sense @pragmatics. This approach to programming simplifies the handling of complex problems: for example, values are inherently persistent and sharable @advanced-data-structures @optimal, immutability prevents data races in concurrent programming, and lambda calculus embodies the core of constructive proofs according to @proofs-as-programs.

However, programming within an immutable framework requires a paradigm shift from traditional imperative programming. Data structures in functional languages are typically built inductively, following an algebraic approach @construction @Pfenning2018 @hottbook. A functional programming language might start with simple built-in types like natural numbers and boolean values, then construct new types as combinations or variations of existing ones. For instance, a `List` structure in Lean 4 could be defined as:
```lean
inductive List : Type :=
  | Nil 
  | Cons (hd : Nat) (tl : List)
```
Here, `List` represents a sum type of `Nil` and `Cons`, with `Cons` being a product type combining `Nat` and `List`.

Given this construction, it's important to note that operations on functional data structures can be defined as functions in the mathematical sense. Consider the following red-black tree in Haskell from @algoxy:
```hs
data Color = R | B | BB
data RBTree = Empty | Node Color RBTree Int RBTree
```
Assume that tree is represented as tuples aliased as type $TT$, $phi$ is a function that rebalance doubly blacked nodes and $mu$ is a function that blackens a node. Within this structure, the deletion operation can be articulated in terms of mathematical functions. 
$
f &: ZZ times TT -> TT\
f(x, t) &= 
cases(
  emptyset\, & t = emptyset,
  phi(C, f(x, l), k, r)\, & t = (C, l, k, r) and x < k,
  phi(C, l, k, f(x, r))\, & t = (C, l, k, r) and x > k,
  r\, & t = (C, l, k, r) and x = k and l = emptyset and C != B,
  mu(r)\, & t = (C, l, k, r) and x = k and l = emptyset and C = B,
  l\, & t = (C, l, k, r) and x = k and r = emptyset and C != B,
  mu(l)\, & t = (C, l, k, r) and x = k and r = emptyset and C = B,
  phi(C, l, m, (f, min(r), r))\, &"otherwise"
)
$

== Introductions and Eliminations

In order to effectively reason about these inductively defined data structures (in the mathematical manner mentioned in the previous chapter), functional programming languages must generate various type-theoretical rules associated with the type definitions. From a computational perspective, the most crucial among these are the introduction rules and the elimination rules. As an interesting comparison, @Lafont1997InteractionC also mentioned that the fundamental laws of computation are commutation (where new combinations are created) and annihilation (where cells are eliminated). In the context of functional programming, introduction rules relate to calls to constructors, facilitating the creation of new instances of a type. On the other hand, elimination rules pertain to the decomposition of values from an inductively defined object, such as through field projections and pattern matching, enabling the extraction and use of the data encapsulated within the structure.

The operations that manipulate these data structures rely heavily on the application of these rules. In the subsequent section, we will explore two additional case studies that illustrate these common patterns, which are extensively utilized in practical scenarios.

=== Red-black Tree Balancing

The provided code from @algoxy demonstrates an operation on the red-black tree, specifically the rebalancing process following an insertion. This code transforms the tree's structure to preserve the invariants that red-black trees must maintain. Implemented in Haskell, this function employs a pattern-matching style:
#text(9pt)[
```hs
balance B (Node R (Node R a x b) y c) z d = Node R (Node B a x b) y (Node B c z d)
balance B (Node R a x (Node R b y c)) z d = Node R (Node B a x b) y (Node B c z d)
balance B a x (Node R b y (Node R c z d)) = Node R (Node B a x b) y (Node B c z d)
balance B a x (Node R (Node R b y c) z d) = Node R (Node B a x b) y (Node B c z d)
balance color l k r = Node color l k r
```
]
In this function, the left-hand side applies pattern matching to deconstruct the tree nodes and checks if their configuration falls into one of the specified categories for rebalancing. The four cases listed address different possible states of imbalance caused by insertion. On the right-hand side, if a potential violation of the red-black tree properties is identified, a new node structure is constructed. This reconstruction uses the fields decomposed from the original nodes on the left-hand side, effectively rebalancing the tree and ensuring that it adheres to the necessary red-black properties. Indeed, as one may notice, this function heavily employs consecutive uses of elimination rules and introduction rules.

=== Normalization of (Partial) Evaluation

Another compelling example is found in the domain of dependent type checkers, which are fundamental to proof assistants. In a dependently typed system, the core language does not make a clear distinction between types and data. As a consequence, for type checking to be executed, proof assistants must (weakly) normalize the language terms @pi-forall @how-to-implement-ddt @how-to-code-your-own-type-theory. A notable strategy to accomplish this is through Normalization by Elaboration (NbE), also known as Normalization by Partial Evaluation (NbPE) @normalization-by-evaluation @NbPE.

The basic idea behind NbPE is to devise a specific semantic interpretation, denoted as 
$lr(bracket.l.double dot.c bracket.r.double)$, that can be efficiently evaluated in the host language. This is coupled with a quote (or reify) operation that converts an object from the semantic interpretation domain back into the language's terms. The interpretation and reification, in a pair, garantee that @NbPE-diagram commutes.

#figure(
  caption: "Diagram of NbPE",
  fletcher.diagram( {
  let (g, f, G, F) = ((-1, 1), (-1, -1), (1, 1), (1, -1));
  node(f, $T$)
  node(g, $T'$)
  node(F, $lr(bracket.l.double T bracket.r.double)$)
  node(G, $lr(bracket.l.double T' bracket.r.double)$)
  edge(f, g, "->", "norm")
  edge(f, F, "->", $bracket.l.double" "dot.c" "bracket.r.double$)
  edge(F, G, "->", "eval")
  edge(G, g, "->", "quote/reify")
})) <NbPE-diagram>

The provided code from @elaberation-zoo showcases a way to apply NbPE to the Untyped Lambda Calculus (UTLC):

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

One can observe that such operations heavily involve the elimination and introduction processes, executed in a consecutive manner.

#reuse-section

= High-level Memory Reuse Runtime

== Reference Counting Runtime without Type Erasure <no-erasure>
Two representative languages implementations that utilize RC-based reuse analysis are Koka @perceus and Lean @lean-4. Albiet the differences in how they approach the reuse analysis, their runtime shares various common features:

1. Both languages are lowered to C or LLVM-IR directly obtained from C code, where memory management is performed with low-level pointer arithmetics.
2. Type are erased. Runtime objects are stored in a uniform way. The runtime does not use the type info to allocate or deallocate objects. Instead, basic information such as number of fields are simply stored in an extra header field associated with the object. Runtime replies on fields scanning to accomplish recursive destruction. 
3. Boxing and unboxing are heavily involed. To manage all language objects in a uniform way, scalar types are either tagged or boxed. This introduces extra indirections and branches during execution.

We propose a higher level runtime implementation targeting the Rust programming language. Instead of erasing types during early stage of the IR manipulation, we hope to investigate the possibility to use higher-order language features to directly manage guest language objects. By implementing such runtime, we also hope to investigate some of the open questions surrounding reuse analysis.

The frontend of a guest language can generate IR to provide function and inductive type definitions. Our framework will perform a series of operations to deliver final output in Rust. As one can see from the demo below, the IR itself can contain meta variables, which makes the frontend translation easier. As a side note, one should also notice that CFG on this IR is of tree structures, as there is no loop in functional programming. When lowering the code to Rust, however, the backend can create loops with tail-call optimization pass.

#text(size: 10pt)[
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

We plan to implement the reuse analysis and code lowering in a multi-pass manner. Some highlights are listed as the following:

1. *type inference*: Infer types for operands. This pass also checks that function calls and basic operations are well-formed.
2. *liveness analysis*: Detect the last-use frontier for operands. After this pass, explicit `drop()` operations will be inserted.
3. *clone analysis*: Check multiple uses on a single managed object and insert `clone()` accordingly.
4. *shape analysis*: Analyze the memory layout of managed objects.
5. *reuse pairing*: Decide proper memory reuse and pair reuse tokens with allocations.
6. *specialization and other optimizations*: Other optimizations such as in-place updates and tail-call recursion.

In the following sections, we highlights the problems we want to investigate with our proposed framework.

== Encoding Memory Reuse with Affline Linearity

Rust language has a unique ownership design to garantee its memory safety. Such ownership system implicity provides similar programming model such as linear types and affine types @rust-affine. Researchers have shown the powerfulness of Rust's ownership system by developping explicit permission separation utilizing invarant lifetime markers as branded access tokens @ghost-cell.

While the powerful ownership checking mechanisms significantly improve memory safety, it makes Rust unfriendly to be a codegen target for guest languages, as the code generator would have extra burden to garantee ownership and lifetime is handled properly.

We noticed similarity between the requirement of Rust and the RC-based memory reuse runtime. As discussed above, to achieve memory reuse, ownership of the RC-objected is transferred to the callee on each function call. If there is any pending use of the same argument object after the functional call, an explicit `clone()` must be inserted to garantee the liveness of the object. Since Rust defaults to move semantic and has a strict ownership checker, it offers free correctness assurance for the generated code. As one will see in the following sections, we will also allow object borrows in foreign language interface (FFI). Rust will also protect us from memory errors in such cases.

== Seamless Interpolation <interpolation>

Let's take a look at a "side effect" of Rc-based reuse analysis. Linearity provides a way to hide side effects in functional programming @linear. In the context of an Rc-based runtime, this approach enables direct manipulation of imperative data structures with minimal additional abstraction. This is illustrated with the following examples:
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
Notice that in the guest language, although the `Vec<i32>` is managed by `Rc` down to the low-level runtime, we do not explicit mark it in the high-level syntax. The `Rc::make_mut()` function is an existing function in Rust's `alloc` library, which is roughly doing the following operation:

```rust 
impl<T : Clone + ?Sized> Rc<T> {
  fn make_mut(&mut self) -> &mut T {
    if Rc::strong_count(self) != 1 || Rc::weak_count(self) != 0 {
      // clone if the RC pointer is not exclusive
      *self = Rc::new(self.clone());
    } 
    // obtain a mutable referece in place
    unsafe { &mut *(&*self as *const _ as *mut _)  }
  }
}
```
Assume we are constructing the initial data structure using `collect 10000 Vec::new`. One should notice that in this fast path, there is no `clone()` needed as the pointer to the object is always exclusive. In this way, `Rc-based` reuse analysis, provides a canonical way to convert a clonable#footnote[We assume that `clone` operation garantees that the original object and the new object should function observably in the same way.] imperative data structure to functional programming language without loosing mutability. 

With Koka and Lean, this method can be used to encapsulate common data structures like dynamic array and hash tables. While being efficient, there are several caveats related to this particular method. The data structure pointer is passed across FFI, and it is up to the user to maintain the correct reference counting operations, which can be tedious and error-prone. 

We hope to investigate the possibility of direct interpolation with host language. When compiling the Rust, we hope to provide a way to automatically generate necessary wrappers for mutable imperative data structures. Given proper trait bounds, such wrappers will also rule out invalid operations.

As suggested in @fp2, with RC-based memory reuse runtime, functions (especially FFIs) may desire borrowed refereces for better performance. For instance, if we pass RC pointers into `length : String -> usize`, the function will have to hold the ownership and generate code related to clean up the string. A better way is to mark the `String` as passed-by-reference, which would end up with a much cleaner code. However, in Koka and Lean's FFI, every object is passed as a pointer without any safety garantee. A carelessly implemented host language function may modify the reference counter or mutate the data fields accidentally and leads to unexpected consequences. 

As readers will see in @uniqueness, borrowing the idea from @Kappa, @Pony and @Verona, we have proposed a fine grained division of argument passing styles with different reference capabilities. Such division works closely with Rust's type system, assuring the correctness across host and guest languages.

== Efficient HOAS

High Order Abstract Syntax (HOAS), as discussed in @hoas, is a methodology to represent guest language structure directly using high-level constructions in host language. For example, instead of using traditional AST to represent the lambda expression, the guest lambda can be directly encoded as a host lambda when feasible. 

Lambda expressions are indeed one of the most complicated runtime features. As first-class construction in functional programming, lambda expression demands sophisticated runtime support, such as variable catpure and partial application. In Lean4, the runtime has a special `lean_closure_object` consisting of plain function pointers and additional heap space for captured objects. Since object's type is erased upon capture, Lean4 created a "uniform ABI" where all values used by the lambda are boxed, including managed objects and scalar values. This creates two further complications. First, an indirection layer for boxed function call must be created for normal functions, as they may also be used as lambdas. Second, passing scalars may result in counter-intuitive overheads including tagging and memory allocation.

We have investigated the possibility of using Rust's closure to directly encode guest's lambda expression. With this setting, Rc-managed objects and scalar values can be directly captured into `Rc<dyn BoxedClosure<P, R>>` object, where `P` is a list of parameters and `R` is the return type. The trait bound of `BoxedClosure<P, R>` is implemented for `Thunk`. The dynamic trait object is desired here. One can associate wrappers around plain functions or closures with the trait bound. In either way, it can be passed or stored uniformly as an object.

```rs
pub struct Thunk<F, P: PartialParams, R>
where
    F: FnOnce(P::Full) -> R + Clone,
{
    code: F,
    params: P,
}
```
As shown in the code above, a `Thunk` is just a Rust `FnOnce` object (it can either be a closure or a plain function closure, but must be decided statically), associated with a tuple of parameters representing the partial application status. 


```rs
impl<T0: Clone, T1: Clone, T2: Clone, T3: Clone> PartialParams
for (Ready<T0>, Ready<T1>, Ready<T2>, Hole<T3>) {
    type Pending = (T3,);
    type Progress = (Ready<T0>, Ready<T1>, Ready<T2>, Ready<T3>);
}
```

Elements inside the parameter tuple is marked with either `Ready` or `Hole` to represent whether the positional argument is supplied or not. The associated type `Progress` denotes the next state to transit when a further argument is supplied. One should notice that this design fits the RC-based reuse analysis properly: when an additional argument is supplied, the closure will check if it holds the exclusive reference to the underlying `Thunk`. If the reference is unique, it either updates he parameter pack in place  or consumes the `code` field direcly, depending on whether this `params` are full. Otherwise, `clone()` operation will be performed. Such `clone()` is shallow since values are captured as `Rc`-managed objects or scalars --- there is no need for recursive `clone()`.

We have experimented the proposed implementation with sample codegen results. Tests are passed under Rust's mid-level IR interpreter @Miri. There is no undefined behavior or other memory errors. 
 
== Uniqueness Type System without Isolation <uniqueness>

The following Lean4 code adds $1.0$ to all elements inside a `FloatArray` that stores elements in a fixed size contiguous memory area. 

```lean
partial def add1(x : FloatArray) : FloatArray :=
  let rec loop (r : FloatArray) (i : Nat) : FloatArray :=
    if h : i < r.size then
      let idx : Fin r.size := ⟨ i, h ⟩
      loop (r.set idx (r.get idx + 1.0)) (i+1)
    else
      r
  loop x 0
```

Notice that we have carefully crafted the code such that there is no boundary checking. Ideally, with trivial tail-call optimization, the above code should be transformed into a loop feasible for SIMD vectorization. Indeed, `leanc` will transform the code above into the following `C` code where updates are happening in loop.

```c
LEAN_EXPORT lean_object *l_add1_loop(lean_object *x_1, lean_object *x_2) {
_start: {
  lean_object *x_3;
  uint8_t x_4;
  x_3 = lean_float_array_size(x_1);
  x_4 = lean_nat_dec_lt(x_2, x_3);
  lean_dec(x_3);
  if (x_4 == 0) {
    lean_dec(x_2);
    return x_1;
  } else {
    double x_5;
    double x_6;
    double x_7;
    lean_object *x_8;
    lean_object *x_9;
    lean_object *x_10;
    x_5 = lean_float_array_fget(x_1, x_2);
    x_6 = l_add1_loop___closed__1;
    x_7 = lean_float_add(x_5, x_6);
    x_8 = lean_float_array_fset(x_1, x_2, x_7);
    x_9 = lean_unsigned_to_nat(1u);
    x_10 = lean_nat_add(x_2, x_9);
    lean_dec(x_2);
    x_1 = x_8;
    x_2 = x_10;
    goto _start;
  }
}
}
```

However, there are several issues stop `add1` from being vectorized. The first problem is that, along the loop, there are two `lean_dec` operation. This is because the loop indice variable can be potentially boxed in Lean. Such operations can be eliminated with our framework, since we will actively avoid boxing and unboxing as described in @no-erasure.

The other problem is less trivial and harder to resolve. `lean_float_array_fset` and `lean_float_array_fget` are mutation operations on imperative data structures. From @interpolation, we have learned such operations incur a check on exclusivity and a potential slow path that clones the underlying data. Vectorizer cannot conduct optimizations due to the existence of these cold routines.

A straightforward solution is to have uniqueness in type system @Uniqueness @LinearUnique. With proper annotations from users, the compiler can statically determine the uniqueness (or exclusivity) of objects. Thus, extraneous checks on uniqueness can be removed together with slow paths. Such type systems, however, usually suffer from the "coloring" problem. Once a function is colored as requiring uniqueness or linearity on its input, it acquires substantial efforts to passing data between "colored" worlds and "uncolored" worlds. @fp2, on the other hand, provides special annotations that let compilers assist users to check the correctness of their uniqueness markers, thus garantees that in-place updates are indeed performed as expected. Such approach, however, does not solve the "coloring" problem.

We propose a more friendly approach to incorporate static uniqueness into our runtime. We also assume that users are capable of specifying uniqueness requirements in performance critial routines. Notice that, our RC-based runtime provides two operations: (1) checking the exclusivity of a RC reference, and (2) obtaining shallow copies of a managed object. These two operations enable easy transitions between "colored" and "uncolored" functions. Upon calling a function that demands uniqueness, the compiler inserts a check on the uniqueness of the objects and use shallow copy to obtain a new exclusively owned object if necessary. In this way, one can lift the checks in loop out of the body, leaving a clean code path for further optimization opportunities. What's more, calling between "colored" and "uncolored" functions do not require explicit users' efforts.

Together with the discussion in @interpolation, our framework supports three sorts of references. Their conversions are detailed in @reference-diagram.

#let ref-diagram = { 
  let RC = (-1, -1)
  let Unique = (-1, 1)
  let Ref = (1, 0)
  fletcher.diagram(
    node-stroke: 1pt,
    edge-stroke: 1pt,
    node(RC, "Rc<T>"),
    node(Unique, "Unique<T>"),
    node(Ref, "&T"),
    edge(RC, Unique, "-|>", [uniquefy], label-pos: 0.1),
    edge(Unique, RC, "-|>", [direct cast], label-pos: 0.1),
    edge(RC, Ref, "-|>", bend: 30deg, [borrow]),
    edge(Unique, Ref, "-|>", bend: -30deg, [borrow]),
  )
}
#figure(
  ref-diagram,
  caption: [Reference Sorts]
) <reference-diagram>

- `Rc<T>` is the traditional RC pointer to a managed object.
- `Unique<T>` can only be used in function parameters (not materializable as object fields). It is used to denote the exclusivity statically as discussed above. A possible runtime implementation is to add a wrapper to the `Rc<T>` with compiler instrinsics hinting the exclusivity (such as `llvm.assume` and `llvm.unreachable`).  
- `&T` represents a borrowed reference to the object. As mentioned in @interpolation, such borrowed references can be used in FFI. When compiling to Rust, the reference is translated as a reference to the underlying value inside the `Rc` managed area. This setting automatically makes sure that safe FFI cannot manipulate the reference counting of the memory object, avoiding interference to the reuse analysis. 

== Open Type Parameters

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
#figure(
text(size: 9pt)[
$
#curryst.proof-tree(basic-eq)   #curryst.proof-tree(basic-neq) 
#curryst.proof-tree(var-sim) \
#curryst.proof-tree(sum-equiv) #curryst.proof-tree(sum-ne-l) #curryst.proof-tree(sum-ne-r) \ #curryst.proof-tree(sum-sim-l) #curryst.proof-tree(sum-sim-r) #curryst.proof-tree(sum-sim) \
#curryst.proof-tree(prod-equiv) #curryst.proof-tree(prod-ne-l) #curryst.proof-tree(prod-ne-r) \ #curryst.proof-tree(prod-sim-l) #curryst.proof-tree(prod-sim-r) #curryst.proof-tree(prod-sim)
$
], caption: [Inference Rules for Shape Analysis])
== Memory Reuse Fusion and Specialization

= Open Problems

== Optimal Pairing for Memory Reuse 
== GC Integration


= Related Work
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Describe related work regarding your topic and emphasize your (scientific) contribution in contrast to existing approaches / concepts / workflows. Related work is usually current research by others and you defend yourself against the statement: “Why is your thesis relevant? The problem was al- ready solved by XYZ.” If you have multiple related works, use subsections to separate them.
]

= Requirements Analysis
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: This chapter follows the Requirements Analysis Document Template in @bruegge2004object. Important: Make sure that the whole chapter is independent of the chosen technology and development platform. The idea is that you illustrate concepts, taxonomies and relationships of the application domain independent of the solution domain! Cite @bruegge2004object several times in this chapter.

]

== Overview
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Provide a short overview about the purpose, scope, objectives and success criteria of the system that you like to develop.
]

== Current System
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: This section is only required if the proposed system (i.e. the system that you develop in the thesis) should replace an existing system.
]

== Proposed System
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: If you leave out the section “Current system”, you can rename this section into “Requirements”.
]

=== Functional Requirements
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: List and describe all functional requirements of your system. Also mention requirements that you were not able to realize. The short title should be in the form “verb objective”

  - FR1 Short Title: Short Description. 
  - FR2 Short Title: Short Description. 
  - FR3 Short Title: Short Description.
]

=== Nonfunctional Requirements
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: List and describe all nonfunctional requirements of your system. Also mention requirements that you were not able to realize. Categorize them using the FURPS+ model described in @bruegge2004object without the category functionality that was already covered with the functional requirements.

  - NFR1 Category: Short Description. 
  - NFR2 Category: Short Description. 
  - NFR3 Category: Short Description.

]

== System Models
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: This section includes important system models for the requirements analysis.
]

=== Scenarios
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: If you do not distinguish between visionary and demo scenarios, you can remove the two subsubsections below and list all scenarios here.

  *Visionary Scenarios*

  Note: Describe 1-2 visionary scenario here, i.e. a scenario that would perfectly solve your problem, even if it might not be realizable. Use free text description.

  *Demo Scenarios*

  Note: Describe 1-2 demo scenario here, i.e. a scenario that you can implement and demonstrate until the end of your thesis. Use free text description.
]

=== Use Case Model
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: This subsection should contain a UML Use Case Diagram including roles and their use cases. You can use colors to indicate priorities. Think about splitting the diagram into multiple ones if you have more than 10 use cases. *Important:* Make sure to describe the most important use cases using the use case table template (./tex/use-case-table.tex). Also describe the rationale of the use case model, i.e. why you modeled it like you show it in the diagram.

]

=== Analysis Object Model
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: This subsection should contain a UML Class Diagram showing the most important objects, attributes, methods and relations of your application domain including taxonomies using specification inheritance (see @bruegge2004object). Do not insert objects, attributes or methods of the solution domain. *Important:* Make sure to describe the analysis object model thoroughly in the text so that readers are able to understand the diagram. Also write about the rationale how and why you modeled the concepts like this.

]

=== Dynamic Model
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: This subsection should contain dynamic UML diagrams. These can be a UML state diagrams, UML communication diagrams or UML activity diagrams.*Important:* Make sure to describe the diagram and its rationale in the text. *Do not use UML sequence diagrams.*
]

=== User Interface
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Show mockups of the user interface of the software you develop and their connections / transitions. You can also create a storyboard. *Important:* Describe the mockups and their rationale in the text.
]

= System Design
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: This chapter follows the System Design Document Template in @bruegge2004object. You describe in this chapter how you map the concepts of the application domain to the solution domain. Some sections are optional, if they do not apply to your problem. Cite @bruegge2004object several times in this chapter.
]

== Overview
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Provide a brief overview of the software architecture and references to other chapters (e.g. requirements analysis), references to existing systems, constraints impacting the software architecture..
]

== Design Goals
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Derive design goals from your nonfunctional requirements, prioritize them (as they might conflict with each other) and describe the rationale of your prioritization. Any trade-offs between design goals (e.g., build vs. buy, memory space vs. response time), and the rationale behind the specific solution should be described in this section
]

== Subsytem Decomposition
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Describe the architecture of your system by decomposing it into subsys- tems and the services provided by each subsystem. Use UML class diagrams including packages / components for each subsystem.
]

== Hardware Software Mapping
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: This section describes how the subsystems are mapped onto existing hardware and software components. The description is accompanied by a UML deployment diagram. The existing components are often off-the-shelf components. If the components are distributed on different nodes, the network infrastructure and the protocols are also described.
]

== Persistent Data Management
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Optional section that describes how data is saved over the lifetime of the system and which data. Usually this is either done by saving data in structured files or in databases. If this is applicable for the thesis, describe the approach for persisting data here and show a UML class diagram how the entity objects are mapped to persistent storage. It contains a rationale of the selected storage scheme, file system or database, a description of the selected database and database administration issues.
]

== Access Control
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Optional section describing the access control and security issues based on the nonfunctional requirements in the requirements analysis. It also de- scribes the implementation of the access matrix based on capabilities or access control lists, the selection of authentication mechanisms and the use of en- cryption algorithms.
]

== Global Software Control
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Optional section describing the control flow of the system, in particular, whether a monolithic, event-driven control flow or concurrent processes have been selected, how requests are initiated and specific synchronization issues
]

== Boundry Conditions
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Optional section describing the use cases how to start up the separate components of the system, how to shut them down, and what to do if a component or the system fails.
]

= Case Study / Evaluation
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: If you did an evaluation / case study, describe it here.
]

== Design 
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Describe the design / methodology of the evaluation and why you did it like that. E.g. what kind of evaluation have you done (e.g. questionnaire, personal interviews, simulation, quantitative analysis of metrics, what kind of participants, what kind of questions, what was the procedure?
]

== Objectives
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Derive concrete objectives / hypotheses for this evaluation from the general ones in the introduction.
]

== Results
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Summarize the most interesting results of your evaluation (without interpretation). Additional results can be put into the appendix.
]

== Findings
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Interpret the results and conclude interesting findings
]

== Discussion
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Discuss the findings in more detail and also review possible disadvantages that you found
]

== Limitations
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Describe limitations and threats to validity of your evaluation, e.g. reliability, generalizability, selection bias, researcher bias
]

= Summary
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: This chapter includes the status of your thesis, a conclusion and an outlook about future work.
]

== Status
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Describe honestly the achieved goals (e.g. the well implemented and tested use cases) and the open goals here. if you only have achieved goals, you did something wrong in your analysis.
]

=== Realized Goals
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Summarize the achieved goals by repeating the realized requirements or use cases stating how you realized them.
]

=== Open Goals
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Summarize the open goals by repeating the open requirements or use cases and explaining why you were not able to achieve them. Important: It might be suspicious, if you do not have open goals. This usually indicates that you did not thoroughly analyze your problems.
]

== Conclusion
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Recap shortly which problem you solved in your thesis and discuss your *contributions* here.
]

== Future Work
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: Tell us the next steps (that you would do if you have more time). Be creative, visionary and open-minded here.
]