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
#set math.equation(numbering: "(1)")

= Introduction

// TODO: enhance citation
Functional programming, as its name suggests, allows users to compose their programs in a way that is similar to mathematical functions. The ability to present programs similar to mathematical expressions lies in the immutable nature of functional programming. Such programming paradigm has several advantages. For instance, immutability implies race free in the context of parallel programming. The simplied control flows and immutable assumptions of functional programs largely simplifies many advanced static analyses, optimizations and program verifications.

Despite its elegance, functional programming also has its downsides. Immutability appears to be a double-edged sword. Many data structures and algorithms are complicated to implement in functional paradigm. Some may even be impossible to maintain its original efficiency when written in a purely functional style. Performance for general situations in functional languages may also be affected as it is no longer possible to apply inplace updates. Objects often need to be frequently constructed or destroyed in whole even though a state change only touchs partial fields.

This survery wants to explore solutions to these problems. By studying exsiting works, we hope to answer two big questions regarding functional programming:

1. *Is it possible to achieve inplace mutability as in imperative programming while keeping the program in functional paradigm?* If this is possible, what efforts are needed for runtime and static analysis?

2. *Is it possible to interpolate between functional and imperative programming without breaking the immutable views of functional programs?* Does such interpolation require extra efforts from programmers?

In order to answer these questions, we arrange the survery in the following structure:

- @functional-programming will introduce some basic backgrounds of functional programming and common design patterns. We will be able to see when and why immutability are causing problems.

- @early-story will summarize traditional methods to handle such immutability issues. This chapter mainly includes two aspects: 1) the definition of *large object* and *aggregate update* problems in functional programming and their solutions; 2) how sophisticated memory managment algorithms mitigate performance panelty due to frequent object constructions and destructions in functional languages.

- @RC-Revisit continues on exploring the solutions but focusing on some newly developed methods based on *reuse analysis*. This chapter will discuss runtime facilities and static analysis required by efficient memory reuse. Besides solving the inplace update problem, it also explains why reference counting (RC) based runtime enables a straightforward way to wrap imperative data structures into functional programs.

- @related-works summarizes similar works related to reuse analysis in @RC-Revisit. It also lists some remaining issues with these solutions. 

- @future-works  will propose potential improvements to the reuse analysis and provide a detailed discussion on implementation difficulties.

= Functional Programming <functional-programming>

Functional programming is typically associated with a paradigm that formulates programs as lambda expressions and views computation as the β-reduction or normalization of lambda terms. In a purely functional framework, evaluations are devoid of side effects, allowing programs to be regarded as "functions" in the mathematical sense @pragmatics. This approach to programming simplifies the handling of complex problems: values are inherently persistent and sharable @advanced-data-structures @optimal, immutability prevents data races in concurrent programming, and lambda calculus embodies the core of constructive proofs according to @proofs-as-programs.

== State Transitions in Functional Programming
Being immutable, however, functional paradigm has its negative implications to performance. Consider the State Monad implementation in Haskell from MTL @mtl @state-monad. Without the capability to mutate individual field, the state transition is achieved by constructing new state objects and passing it to the desired continuation (the lambda expression wrapped in `state` constructors). If the code were not properly optimized, each of such state transition is accompanied by deallocations of original state object, allocations of new state object, and necessary field data copying. This will lead to considerable performance impacts especially when such object are large enough. 

```haskell
class Monad m => MonadState s m | m -> s where
    -- | Return the state from the internals of the monad.
    get :: m s
    get = state (\s -> (s, s))

    -- | Replace the state inside the monad.
    put :: s -> m ()
    put s = state (\_ -> ((), s))

    -- | Embed a simple state action into the monad.
    state :: (s -> (a, s)) -> m a
    state f = do
      s <- get
      let ~(a, s') = f s
      put s'
      return a
``` <state-monad-code>

== Functional Data Structures

Unfortunately, consecutive construction and destruction of objects is actually a common pattern of functional paradigm.

#text(9pt)[
```hs
balance B (Node R (Node R a x b) y c) z d = Node R (Node B a x b) y (Node B c z d)
balance B (Node R a x (Node R b y c)) z d = Node R (Node B a x b) y (Node B c z d)
balance B a x (Node R b y (Node R c z d)) = Node R (Node B a x b) y (Node B c z d)
balance B a x (Node R (Node R b y c) z d) = Node R (Node B a x b) y (Node B c z d)
balance color l k r = Node color l k r
```
]

Consider the functional red-black tree balancing algorithm implemented by @algoxy. The implementation uses pattern matching syntax in Haskell. On the left-hand side, the functions check if the arguments conforming certain patterns. If a match is found, balanced nodes on the right-hand side are constructed as return values using matched values from left-hand side. Implicitly, such procedure indicate the arguments on the left-hand side can be destructed if there is no further reference to these values after execution. The right-hand side nodes are newly constructed, hence demand allocations. One can imagine that the functional implementation may lead to much more performance penalty due to the "immutable nature" of the paradigm.

== The Demands for Performance

Before we move on to the solutions, let's answer the following questions: 
1. Why does the performance of functional languages matter? 
2. To what extend does the immutability affects the overall efficiency?

In order to answer these questions, let's conduct an experiment on the Lean4  @lean-4 proof assistant. Lean4 and other dependent-typed functional programming languages are attracting increasingly more interests among cryptography, certified compilation and mathematics community. Compiler researchers and cryptography researchers are using them to verify critial programs. Mathematicians are embedding proof assistants into their workflows to tackle some most challenging works such as formalizing proofs to the Fermat's Last Theorem for regular primes @flt.

For proof assistants, their type systems are rather complicated, which usually require bidirectional type checking @pi-forall @how-to-implement-ddt @how-to-code-your-own-type-theory and NbPE @NbPE techniques on a relatively large ruleset. Many proof assistants choose to boostrap themselves @lean-4 @agda. On one hand, the type checking rules can be closely represented in functional programming as the rules are typically described in typed lambda calculus. On the other hand, doing so provides a measure for these proof asisstants to assess their own correctness. The type checking algorithms mentioned above exhibits the common patterns of functional programming and thus affected by the performance implications. 

`mathlib4` @mathlib4 is a community driven project to formalize as much as mathematics with Lean4. As the project grows bigger and bigger, compilation time (dominated by type checking mathematical theorems) becomes increasingly concerning. There are $4886$ targets inside the project up to the time of access. Due to the long time of compilation, Lean4 community has to distribute "elaberated" #footnote("To check type equivalence, expressions are elaborated into certain normal forms via NbPE. Such an algorithm works like an interpreter that translates lambda terms into a semantic domain, evaluates it in such domain and converts them back to concrete lambda terms. The process manipulates a large amount of AST nodes in two different domains.") cache files to skip over some most time-consuming parts inside the compilation pipeline.

We evaluate the time of full `mathlib4` compilation (not using any cache) with and without the Small Allocator. Lean4 is using some advanced techiniques to mitigate performance penalty due to immutability. The Small Allocator is one of its means to reduce the cost due to frequent allocations (which reflects the impact of immutability) by caching small memory blocks aggressively. We will make a more detailed discussion in upcomming sections. For now, the most important takeaway is that the difference in compilation time can reflect the extend to which allocations are affecting the performance of functional programs. The device we used is Surface Pro 11 with 12 Qualcomm X Elite AArch64 cores. It is of a high-end profile for daily works. We allow the Lean4 compiler to utilize all 12 cores to parallelize the process.

#let profiling = {
  set align(center)
  set table(
    stroke: none,
    gutter: 0.2em,
    fill: (x, y) =>
      if x == 0 or y == 0 { gray },
    inset: (right: 1.5em),
  )

  show table.cell: it => {
    if it.x == 0 or it.y == 0 {
      set text(white)
      strong(it)
    } else if it.body == [] {
      // Replace empty cells with 'N/A'
      pad(..it.inset)[_N/A_]
    } else {
      it
    }
  }

  let a(x) = table.cell(
    fill: green.lighten(60%),
  )[#x]
  let b(x) = table.cell(
    fill: aqua.lighten(60%),
  )[#x]

  table(
    columns: 3,
    [], [w/ Small Allocator], [w/o  Small Allocator],
    [Time], a("3933.59s"), b("5010.23s")
  )
};

#profiling

One can infer from the profiling data that 1) compilation time is a big concern for large functional projects, and 2) allocation optimization is one of the effective way to make the progress faster, leading to over 27% of speed up in `mathlib4` full compilation. In following chapters, we will see how Lean4 and other functional languages leverage various memory management tricks to deliver better performance.


// = Introduction (Old)
// In recent years, proof assistants have surged in popularity across both academic and industrial domains. Computer scientists begin to use proof assistants to verify realistic software systems as exemplified by @Leroy-Compcert-CACM @composable-verification. Simultaneously, mathematicians are beginning to seriously integrate computer-aided proofs into their workflows @terence_tao_lean_tour @Fermat_last_theorem. Such trend leads to serious considerations for the design and implementation of high-performance functional programming languages.

// An emerging challenge lies in efficiently managing memory resources for functional languages. Essentially, the memory management policy must address rapid allocations and deallocations to enhance memory reuse and locality. In a general context, global allocators strive to adapt to these patterns by establishing various levels of free lists, as discussed in @locality-alloc @leijen2019mimalloc @snmalloc. Functional programming, distinguished by its inherent immutability, requires even more frequent memory reuse to accommodate its specific needs.

// Efficient memory management and locality optimization remain central to the evolution of modern programming languages and software development. The memory resources of functional languages are typically managed by garbage collectors. Depending on different user scenarios, various implementations may configure their collection algorithms with distinct characteristics, as evidenced by the diverse approaches in @haskell, @ocaml, @ocaml-pm, @erlang-1, and @erlang-2. Despite these differences, these advanced collection algorithms universally adopt generational approaches, underscoring the significance of locality and efficient memory reuse in frequently accessed (hot) regions. Recently developed languages like Koka and Lean 4 have illuminated the potential of combining Rc-based (Reference Counting) runtime and reuse analysis to achieve in-place mutability within a purely functional environment, as indicated by @lean-4 @perceus @frame-limited @fp2. Researchers have termed this innovative approach as the "functional-but-in-place" (FBIP) paradigm. 

// This work examines the characteristics of functional programming alongside previous research in the domain of memory management and reuse analysis. By conducting case studies, this study aims to provide a comprehensive understanding of the essence of memory reuse, as well as the advantages and disadvantages of the new Rc-based methods. Furthermore, by proposing a high-level runtime in Rust, this work will suggest potential enhancements to address existing challenges in reuse analysis and the Rc-based runtime framework.

// = Functional Programming
// == A Brief Overview
// Functional programming is typically associated with a paradigm that formulates programs as lambda expressions and views computation as the β-reduction or normalization of lambda terms. In a purely functional framework, evaluations are devoid of side effects, allowing programs to be regarded as "functions" in the mathematical sense @pragmatics. This approach to programming simplifies the handling of complex problems: for example, values are inherently persistent and sharable @advanced-data-structures @optimal, immutability prevents data races in concurrent programming, and lambda calculus embodies the core of constructive proofs according to @proofs-as-programs.

// However, programming within an immutable framework requires a paradigm shift from traditional imperative programming. Data structures in functional languages are typically built inductively, following an algebraic approach @construction @Pfenning2018 @hottbook. A functional programming language might start with simple built-in types like natural numbers and boolean values, then construct new types as combinations or variations of existing ones. For instance, a `List` structure in Lean 4 could be defined as:
// ```lean
// inductive List : Type :=
//   | Nil 
//   | Cons (hd : Nat) (tl : List)
// ```
// Here, `List` represents a sum type of `Nil` and `Cons`, with `Cons` being a product type combining `Nat` and `List`.

// Given this construction, it's important to note that operations on functional data structures can be defined as functions in the mathematical sense. Consider the following red-black tree in Haskell from @algoxy:
// ```hs
// data Color = R | B | BB
// data RBTree = Empty | Node Color RBTree Int RBTree
// ```
// Assume that tree is represented as tuples aliased as type $TT$, $phi$ is a function that rebalance doubly blacked nodes and $mu$ is a function that blackens a node. Within this structure, the deletion operation can be articulated in terms of mathematical functions. 
// $
// f &: ZZ times TT -> TT\
// f(x, t) &= 
// cases(
//   emptyset\, & t = emptyset,
//   phi(C, f(x, l), k, r)\, & t = (C, l, k, r) and x < k,
//   phi(C, l, k, f(x, r))\, & t = (C, l, k, r) and x > k,
//   r\, & t = (C, l, k, r) and x = k and l = emptyset and C != B,
//   mu(r)\, & t = (C, l, k, r) and x = k and l = emptyset and C = B,
//   l\, & t = (C, l, k, r) and x = k and r = emptyset and C != B,
//   mu(l)\, & t = (C, l, k, r) and x = k and r = emptyset and C = B,
//   phi(C, l, m, (f, min(r), r))\, &"otherwise"
// )
// $

// == Introductions and Eliminations

// In order to effectively reason about these inductively defined data structures (in the mathematical manner mentioned in the previous chapter), functional programming languages must generate various type-theoretical rules associated with the type definitions. From a computational perspective, the most crucial among these are the introduction rules and the elimination rules. As an interesting comparison, @Lafont1997InteractionC also mentioned that the fundamental laws of computation are commutation (where new combinations are created) and annihilation (where cells are eliminated). In the context of functional programming, introduction rules relate to calls to constructors, facilitating the creation of new instances of a type. On the other hand, elimination rules pertain to the decomposition of values from an inductively defined object, such as through field projections and pattern matching, enabling the extraction and use of the data encapsulated within the structure.

// The operations that manipulate these data structures rely heavily on the application of these rules. In the subsequent section, we will explore two additional case studies that illustrate these common patterns, which are extensively utilized in practical scenarios.

// === Red-black Tree Balancing

// The provided code from @algoxy demonstrates an operation on the red-black tree, specifically the rebalancing process following an insertion. This code transforms the tree's structure to preserve the invariants that red-black trees must maintain. Implemented in Haskell, this function employs a pattern-matching style:
// #text(9pt)[
// ```hs
// balance B (Node R (Node R a x b) y c) z d = Node R (Node B a x b) y (Node B c z d)
// balance B (Node R a x (Node R b y c)) z d = Node R (Node B a x b) y (Node B c z d)
// balance B a x (Node R b y (Node R c z d)) = Node R (Node B a x b) y (Node B c z d)
// balance B a x (Node R (Node R b y c) z d) = Node R (Node B a x b) y (Node B c z d)
// balance color l k r = Node color l k r
// ```
// ]
// In this function, the left-hand side applies pattern matching to deconstruct the tree nodes and checks if their configuration falls into one of the specified categories for rebalancing. The four cases listed address different possible states of imbalance caused by insertion. On the right-hand side, if a potential violation of the red-black tree properties is identified, a new node structure is constructed. This reconstruction uses the fields decomposed from the original nodes on the left-hand side, effectively rebalancing the tree and ensuring that it adheres to the necessary red-black properties. Indeed, as one may notice, this function heavily employs consecutive uses of elimination rules and introduction rules.

// === Normalization of (Partial) Evaluation

// Another compelling example is found in the domain of dependent type checkers, which are fundamental to proof assistants. In a dependently typed system, the core language does not make a clear distinction between types and data. As a consequence, for type checking to be executed, proof assistants must (weakly) normalize the language terms @pi-forall @how-to-implement-ddt @how-to-code-your-own-type-theory. A notable strategy to accomplish this is through Normalization by Elaboration (NbE), also known as Normalization by Partial Evaluation (NbPE) @normalization-by-evaluation @NbPE.

// The basic idea behind NbPE is to devise a specific semantic interpretation, denoted as 
// $lr(bracket.l.double dot.c bracket.r.double)$, that can be efficiently evaluated in the host language. This is coupled with a quote (or reify) operation that converts an object from the semantic interpretation domain back into the language's terms. The interpretation and reification, in a pair, garantee that @NbPE-diagram commutes.

// #figure(
//   caption: "Diagram of NbPE",
//   fletcher.diagram( {
//   let (g, f, G, F) = ((-1, 1), (-1, -1), (1, 1), (1, -1));
//   node(f, $T$)
//   node(g, $T'$)
//   node(F, $lr(bracket.l.double T bracket.r.double)$)
//   node(G, $lr(bracket.l.double T' bracket.r.double)$)
//   edge(f, g, "->", "norm")
//   edge(f, F, "->", $bracket.l.double" "dot.c" "bracket.r.double$)
//   edge(F, G, "->", "eval")
//   edge(G, g, "->", "quote/reify")
// })) <NbPE-diagram>

// The provided code from @elaberation-zoo showcases a way to apply NbPE to the Untyped Lambda Calculus (UTLC):

// ```hs   
// eval :: Env -> Tm -> Val
// eval env = \case
//   Var x     -> fromJust $ lookup x env
//   App t u   -> eval env t $$ eval env u
//   Lam x t   -> VLam x (\u -> eval ((x, u):env) t)
//   Let x t u -> eval ((x, eval env t):env) u

// quote :: [Name] -> Val -> Tm
// quote ns = \case
//   VVar x                 -> Var x
//   VApp t u               -> App (quote ns t) (quote ns u)
//   VLam (fresh ns -> x) t -> Lam x (quote (x:ns) (t (VVar x)))
// ```

// One can observe that such operations heavily involve the elimination and introduction processes, executed in a consecutive manner.

// #reuse-section

= Aggregate Update in Functional Programming: An Early Story <early-story>

Unique properties including referential transparency has been attracting people from various communities to introduce functional languages into their research and production environments. As such, a primary challenge is to workaround the performance issues implied by immutability. In this chapter we focus on summarizing different approaches to tackle the so called "Aggregate Update Problem" (also known as "Large Object Problem") in functional programming environments.

== What is the "Aggregate Update Problem"?

Following the convention in @aggregate-problem, aggregate data types typically refers to data structures that collects an amount of elements in a dense, usually contiguous memory area, such as (multi-dimensional) arrays and strings. 

In an imperative environment, these data structures provide handy ways to store and access elements in an uniform manner with excellent locality. In the functional world, however, one cannot mutate the underlying memory objects (in user-visible ways). Otherwise, referential transparency can potentially be destroyed. Purely functional expressions are devoid of side effects. Many implementations utilize this fact to avoid repeated evaluations of the same expressions. If some memory objects referred by an expression are mutated in unexpected ways, the whole program may produce undesired results.

As shown in @functional-programming, to represent state transitions, functional programs carrying aggregate state objects as the arguments to their continuation. The most safe and conservative way to update aggregate objects is to clone them in whole with required change applied and then pass newly created objects to the continuation. If there are any evaluation referring to old objects, their existing results can still be used as nothing is changed in their memory state.

```haskell
addOne : Array Int -> Int -> Array Int
addOne arr idx = 
  if idx >= len arr 
  then arr 
  else let elm = get arr idx in addOne (set arr idx (elm + 1)) (idx + 1)
```
For instance, in the above code, the builtin function `set` can be implemented to allocate a new array, copying the original array with the element at the provided index modified. The performance issue is immediately apparent. Increasing all elements in a linear array becomes a $O(n^2)$ operation with additional penalty on memory management.

== Solutions to the Aggregate Update Problem
Many solutions are proposed to workaround such issues. One mitigation is to use specially designed functional data structures. The intuiation is that the "Aggregate Update Problem" only happens on "Large Objects". One can break down the data structure into small pieces thus localize the impact of modifications. Associated algorithms create updated objects with mutation happening on a small subset of the sharded pieces while reuse most unchanged parts that can be easily tracked via few pointers. Fingertrees, for example, are typically used as "arrays" in the functional world @algoxy @purely-functional-data-structures. One of their variants is adopted into the standard library of Scala with an amortized extra cost that is almost negligible under a wide range of workloads @scala-pr @rrb-vector.

Efficient drop-in replacements for imperative data structures may not always be available. Pointer chasing implied by large object sharding can affect cache friendliness negatively, thus introduces costs that are difficult to be amortized elegantly. More importantly, general usability demands researchers to find out a non-intrusive method, such that programmers can write functional programs in simple and productive ways without designing sophisticated ad-hoc algorithms while the compilers can produce well-optimized targets that workaround the "Aggregate Update Problem" automatically.

@aggregate-problem suggests a multi-leveled solution using both static analysis and runtime support. Inplace mutations are forbid in functional languages as they may break referential transparency and produce side effects that invalidate the evaluation of other expressions. However, it is implied that if a compiler has the knowledge that a mutation will never produce visible side effects#footnote("Strictly speaking, allocations, locality, and other low-level metrics are observable side effects. They are ignored in most context including this survey as they do not affect the evaluation."), then the freedom should be granted to utilize such mutations for optimizations. A particular example arises when a reference to an object proves to be exclusive. If an aggregate object has its last reference being used for a functional update which creates new object with required fields updated without carrying out any access to the old one, then the compiler can infer that the original object is no longer reachable after the operation. Utilizing the underlying memory becomes legal as external observers do not care about exact memory locations of objects.

 For an aggregate data structure, the compiler first tries to check statically if an update to such aggregate is operating on its last reference. If so, instead of emitting copying, the update can be applied inplace as imperative programs. Such analysis can be done in a context-sensitive manner, inlining and duplicating code pieces to select more applicable sites. Necessary code motions can be combined into the optimization, alternating order of evaluation to achieve maximal inplace updates. When all the means run into a dead end, runtime checking of reference count can kick in to capture the last opportunity of memory reuse.

 The simplicity of functional programs contributes to the static analysis. It is easy to build a dataflow diagram of values in functional programs, referred as "graph-reduction" in @aggregate-problem. For an aggregate type of interest, one can pick up the set of operations that construct and use corresponding objects. Arrays, for example, can be produced by its constructors and mutators and used by projections that access member fields.   

#let dataflow-diagram = { 
  let Update = (0, -1)
  let Get = (0, 1)
  let Proj = (1, 0)
  let NewArr = (-1, 0)
  fletcher.diagram(
    node-stroke: 1pt,
    edge-stroke: 1pt,
    node(Update, "set a 0 5"),
    node(Get, "set a 2 7"),
    node(Proj, "get b 1"),
    node(NewArr, "Array.new [1, 2, 3]"),
    edge(NewArr, Get, "-|>"),
    edge(NewArr, Update, "-|>"),
    edge(Update, Proj, "-|>"),
  )
}

#figure(
  dataflow-diagram,
  caption: [Simple Dataflow Diagram for Functional Array Objects]
) <dataflow-diagram>

In a functional environment, such dataflow diagram should be a DAG (directed acyclic graph). Without mutable references, it is impossible to establish backedges. Possible evaluation orders can be inferred from the dataflow diagram as topological orders. An update can happen inplace if at the time of its evaluation, all other expressions depending on the original objects are evaluated. This was specified in following temporal logic (@temporal-logic) as in @aggregate-problem:

$
forall s in C \\ {u}, square.stroked ("Completed"(u) => "Completed"(s)) \
forall s in C \\ {u}, square.stroked ("Completed"(u) => not (diamond.stroked "Completed"(s)))
$ <temporal-logic>

where $C$ is the set of all uses and $u$ is the update operation.

The analysis can be applied statically during compilation, eliminating copies without special runtime support. If there are multiple possible evaluation orders of a dataflow diagram, one can schedule the evaluation to maximize the opportunity for inplace update. As we have summarized previously, if all these means fail, one can still utilize reference counting to capture the exclusivity.

== Limitations of the Static Analysis Approach

There are several limitations of the static analysis approach:

- First, in order for @temporal-logic to work, the possible set of operations must be well-defined. For arrays, we know that a use is either another update or an element projection. These operations garantee that changing the array does not affect the evaluated value.
- Second, the work in @aggregate-problem uses RC (reference counting) as the last method to avoid unnecessary copying for aggregate types without considering optimizing the RC operations themselves. It concludes RC as a method that introduces extra cost on fast paths. To avoid such cost, the destructions of RC-mananged objects are usually deferred, which makes it incompatible with the purpose of inplace updates. We will revisit the RC-based approaches in next chapter.

The first limitation also applies to some other works like @SISALSA. @SISALSA has a sophisticated system to optimize functional languages with streaming and iterations. As such, it also focus on a fixed set of operations that are important for the purposes of streaming and iterations.

== Memory Management Optimization in General <mem-management>

Previous sections in this chapter demonstrates how researchers tackle the Aggregate Update Problem directly. However, other small objects and composite types that is not "considered" by previous inplace update analysis still face the performance penalty due to frequent constructions and destructions. Let's step backward a little bit and move our view to a more general setting: how one can use memory management algorithms to speed up programs that demands frequent memory resource acquisition and release.

In @functional-programming, we evaluated how the Small Allocator feature in Lean4 affects the performance. Small Allocator is a form of locally-cached freelist widely used in many modern allocators. Localized freelist cache with sophisticated sharding strategy is used in Mimalloc @leijen2019mimalloc and SnMalloc @snmalloc. Similar to Lean4 @lean-4, Mimalloc is playing a crucial role in the runtime of the Koka programming language @Koka. 

Userspace memory pages are acquired from OS via syscalls. However, one cannot always utilize syscalls to obtain memory as syscalls only operate on page level and the roundtrip between userspace and kernel space proves to be costly especially when such trips are taken frequently. Allocators such as those inside the system-wide C libraries behave as a cache layer between applications and kernels. They defer the returning of acquired pages to OS. When applications apply for memory, instead of allocating pages from OS, allocators return available blocks from freelists if there are suitable candidates. Modern allocators further shards freelists. In both MiMalloc and SnMalloc, 1) different freelists can be maintained for different size classes, and 2) besides the global memory pool, separate freelists can be installed into thread-local storage. Imagine the consecutive deallocations and allocations in functional programming. Memory blocks can be pushed into the local freelist upon deallocations and reused immediately for incomming allocations, providing a similar effect for inplace mutations. However, it is important to notice that, the allocations and deallocations must happen in time. Deferred deallocations, as used in many implementations to speed up fastpaths, may defeat the efforts of freelists to some extend.

Similar strategies and ideas are used in various managed language runtimes. @haskell described the block-structured heap for the Haskell programming language. @rc-immix @lxr showcases that a complicated generational GC can utilize a partial RC encoded in a few bits to manage objects with short lifespan, which can be rapidly reclaimed and reused without going though extra stages.

= Revisit Reference Counting <RC-Revisit>

We have summarized some static analysis solutions to the Aggregate Update Problem in previous chapters. These analysis, however, failed to break the boundary of RC (Reference Counting) operations. Therefore, RC is traditionally considered as a slow and fallback solution rather than the main focus of previous studies. In this chapter, we look into recent progress on introducing RC as primary operations of the runtime. As one will see, these new approaches make RC a reasonable choice for high-performance runtime implementation. The simplicity of RC-managed object also allows safe and efficient interpolation across the boundary between functional and imperative world.

== Reference-Counted RAII Objects

Instead of jumping into RC-oriented memory reuse analysis, let's first understand how RC is conventionaly used. One typical scenario is to use reference counting to manage RAII (Resource Acquire Is Initialization) Objects, such as `std::shared_ptr` in C++ or `std::rc::Rc` in Rust. The initial constructors of such RC pointers are in charge of allocating the associated memory resource, the copy constructors are used to increase the reference count, and the destructors are responsible for decreasing the count and releasing the memory if last reference is to be released.

```rust 
fn and_one(list: Rc<List<i32>>, acc: Rc<List<i32>>) -> Rc<List<i32>> {
  match &*list {
    List::Cons(hd, tl) => {
      let updated = Rc::new(List::Cons(hd + 1, acc));
      add_one(tl.clone(), updated)
    },
    List::Nil => acc
  }
  // drop(acc); drop(list);
}
```

Usually, if a RAII object is passed into a function or created inside a function, the function takes the responsibility to clean up the resource if needed. In the case of RC pointers in most languages, the compiler inserts calls to their destructors when exiting their lexical scope. For example, in the demo above, `drop` functions are implicitly invoked before the function returns. 

The clean up routines are deferred such that one can have a clean fastpath. The main body of the function can possibly include no calls to allocators and RC-related checkings or branches. However, as discussed in @mem-management, deferred memory reclamation can cause issues on efficient memory reuse.

We now face a tricky problem. We want our fastpaths to be devoid of cold deallocation routines. In the same time, we also want to utilize RC to achieve memory reuse as it captures the exclusivity precisely.

== Reference Counting as First-class Operations

To tackle the problem mentioned above, @ullrich2020countingimmutablebeansreference introduced `inc`, `dec`, `reset` and `reuse` operators into the IR of the Lean4 programming language. This is a novel idea in functional programming context. For each expressions, we not only focus on how the value is computed but also explicitly mark up the associated operations used to maintain reference counts and manage memory resource.

```haskell
map f xs = case xs of
  | Nil -> ret xs
  | Cons -> 
    let x = proj xs 0;
    let y = proj xs 1;
    inc y;
    let w = reset xs;
    let z = apply f x;
    let zs = map f y;
    let r = reuse w in Cons z zs;
    ret r
```
Given a program composed with traditional IR operations (including projections, applications and constructor calls), the compiler can statically infer the required RC operations. Once explicit RC operations are obtained, more optimization opportunities are exposed to the compiler. For example, in the code above, the compiler notice that the memory associated with `xs` can possibly be reused. Hence, instead of allocating memory for constructors, a "destructive decrement" (`reset` operation) is inserted. A later on `reuse` operation can then avoid allocations if the memory pointer passed in is not null. 

The process proposed in @ullrich2020countingimmutablebeansreference involves three steps:
1. Inplace update operations including `reset` and `reuse` are inserted into feasible sites.
2. Some functions may not need to use RC pointers as they only require immutable projections or other trivial operations on their input arguments. Such functions can be converted to "borrow semantics" to avoid RC operations.
3. With destructive operations and borrowing operations inserted, it becomes straightforward to add necessary reference counting operations.

There are still other challenges such as how to tweak the functional data structures such as red-black trees to better fit into the inplace update scheme and how to efficiently maitain the reference counting in multi-threaded environments. These implementations details are discussed in @ullrich2020countingimmutablebeansreference. We skip them here to focus on the main topics.

Lean4 is implemented based on @ullrich2020countingimmutablebeansreference. Compared with other functional language implementations including `GHC`, `ocamlopt`, `MLton`, `MLKit` and RC-managed language such as `Swift`, `Lean4` can achieve best average performance across different workloads #footnote([
  @ullrich2020countingimmutablebeansreference benchmarked binary trees, symbolic differentiation, constant folding, quick sort and red-black tree based association map.
]). The decomposed measurements shows that the `reuse/reset` insertion pass alone brings about $38%$ speedup compared with the baseline implementation.

== Perceus: Garbage Free Reference Counting with Reuse
In @ullrich2020countingimmutablebeansreference, reference counting maintainence operations are inserted as the fallback of destructive inplace update. It turns out that these operations are subject to further optimization opportunities. In this section, let's take look at how Koka @Koka improves the performance surrounding RC operations @perceus.

=== Destructive Move: A New Ownership Model

Before jumping into the analysis and optimization algorithms, we first need to refresh our mindset towards RC pointers. Holding a RC pointer grants the access to the underlying managed object. All RC holders have a shared ownership of the associated memory resource. 

An interesting question is how to understand passing RC pointers to structure fields or callees. Due to historical absence of move semantics in C/C++, the usual mental model behind such operations is to create a new share of the ownership for new recipients. The reference count is increased and a new RC pointers is duplicated from the original one while keeping the original RC pointer valid.

This leads to several problems, especially for efficient reuse of the memory resource. Even if no explicit operations exist after passing a RC pointer to a subroutine, current call frame implicitly holds a share of the ownership, obstructing the tail call optimization and more importantly, making the memory resource impossible to be reused during the subroutine call.

Unlike C/C++, Rust @rust-affine programming language defaults to destructive move semantics. Once a RC pointer is passed to subroutines or structures, it is no longer accessible from current frame. The responsibility of maintaining reference count and destruction is also moved from current call frame to new "share holders". If accessibility is really needed after a destructive move, a copy of the RC pointer  should be explicitly created before the move.

It is arguable that move semantic is more beneficial in the management of RC objects. It solves the issue of holding of the RC pointers uselessly. In functional programming, however, we normally don't expect an expression to become inaccessible after its first usage#footnote("We will discuss about linearity very soon."). However, it is analyzable whether a value will be used again after a move. Hence, it is possible to default on moving the RC ownership while inserting necessary clone operation on need.

=== Reuse Analysis under Destructive Move
In @ullrich2020countingimmutablebeansreference, the algorithms begins with inserting `reset` and `reuse`. With destructive move, the steps are reordered in @perceus. Given a spartan lambda algebra, one can first insert the `inc` and `dec` operations in the first step, assuming move semantics. Notice that the reference count can now be decreased as soon as its associated pointer is no longer used anywhere in current frame.

For instance, consider following program:
```haskell
map f xs = match xs with
  | Nil => return xs
  | Cons y ys => 
    let y' = f y
    let ys' = map f ys
    let xs' = Cons y' ys'
    return xs' 
```
The `inc` and `dec` operations can be inferred automatically. Notice that we consider pattern matching as a destructive operation, after which the decrement for the corresponding RC pointers should be inserted if the object is used anymore. The above program will be annotated as the following:
```haskell
map f xs = match xs with
  | Nil => 
    dec f; 
    return xs
  | Cons y ys => 
    inc y; inc ys; dec xs;
    inc f;
    let y' = f y
    let ys' = map f ys
    let xs' = Cons y' ys'
    return xs' 
```
At this stage, no inplace update is inserted and we can potentially have decrements on fastpaths resulting in bad performance implications.

The magic is hidden inside the `dec` operation, which can be expanded into the following form in the `Cons` branch:
```haskell
inc y; inc ys;
if xs.rc == 1
then dec y; dec ys; free xs;
else xs.rc -= 1;
inc f;
let y' = f y
let ys' = map f ys
let xs' = Cons y' ys'
return xs' 
```
One may have noticed that we have consecutive `inc/dec` operations. Why not permeate them into nested control flow and cancel them with each other? This is demonstrated in the following code:

```haskell
if xs.rc == 1
then free xs;
else inc y; inc ys; xs.rc -= 1;
inc f;
let y' = f y
let ys' = map f ys
let xs' = Cons y' ys'
return xs' 
```
We have a much more clean fastpath now. However, there is a cold `free` operation remained. Looking ahead, we can see there is a constructor call. The associated memory resource of `xs` can be directly handed over to `xs'`. Hence, instead of using `free`, we explicitly make the expanded `dec` operation return a memory token and passing it to the constructor call:
```haskell
let token = 
  if xs.rc == 1
  then address of xs;
  else inc y; inc ys; xs.rc -= 1; null
inc f;
let y' = f y
let ys' = map f ys
let mem = 
  if token is null then alloc else token
let xs' = Cons@mem y' ys'
return xs' 
```
On the fastpath where `xs` holds the exclusive reference, there can be no RC operations and no allocator calls other than checking the reference count.

== Drop-Guided Reuse Analysis
The reuse analysis apporach can be further simplified and improved. In @perceus, the `dec` operation can be expanded in two different ways. One is to free the memory resource directly while the other one is to give out the memory resource for future reuse. The decision is made by the compiler depending on whether it finds a reuse opportunity somewhere ahead of the decrement.

Such decisions, however, may not be easy to made and can degrade performance if not handled carefully. Even if memory reuse is possible, the appropriate insertion site for reuse token creation may not be trivial to determine. Consider the following nested pattern matching from @frame-limited:

```haskell
match x with
  | Just(_) => // x is still live here
    match y with
    | 0 -> x
    | _ -> Just(y)
```

@perceus considers the pattern matching as a destructive operation, hence, the possible insertion site for reuse token is right after the first match. Unfortunately, the liveness of `x` inside nested branches make it impossible to reuse the memory. 

Is it possible to push down the `dec` operation into nested control flows? Ideally, such code motion will solve the above problem:

```haskell
match x with
  | Just(_) => // x is still live here
    match y with
    | 0 -> dec(y); x
    | _ -> let w = reset(x); Just@w(y)
```

However, postponed `dec` operation can defeat the timeliness of RC memory management. This may not be immediately apparent in the above scenario but one can consider the situation where there is another subroutine call between the `dec` and the candidate constructor call feasible for memory reuse.

```haskell
match xs
  Cons(_,_) =>
    inc xs;
    let y = f(xs)
    let r = reset(xs)
    Cons@r(y,Nil)
```
Since the compiler wants to enforce the memory reuse, it optimistically inserts an increment operation to allow passing `xs` to subroutine `f` while postponing the destructive decrement `reset` until the return of `f`, such that the constructor call can grab the chance of inplace memory reuse. The side effect of the code motion is that `xs` is kept alive during the whole lifetime of the routine call, disabling possible reuse inside the nested functions and possibly leading to nondeterministic heap growth in the worst situation!

All these issues show that looking ahead for constructor calls to decide whether or where a destructive decrement is to be installed may not be a favorable solution. This leads to the proposal from @frame-limited. 

It is suggested that the compiler should not distinguish the decrement in two different operations. Similar to the first step analysis in @perceus, `inc/dec` operations are inserted without the consideration of reuse. The `dec` operation denotes that some memory resource possibility becomes available within the context. We can do a backward dataflow analysis to populate possible allocations along the control flow. We carry the possibly available "resource" along the CFG, until it is paired with a suitable allocation at a contructor call or we no longer find any feasible allocation along the control flow. In the latter, we insert an explicit free operation of the memory resource. Another way to think about the algorithm is that we always insert a destructive decrement instead of normal `dec` and insert free if it is not paired with any reuse operation.

In the situation of nested reuse, the new approach automatically "push down" the decrement operation into branches. In the situation where subroutine calls consuming a RC-managed object, the `dec` operation will not be inserted in the first place, hence the issues caused by deferred memory release no longer exist. The new approach has a nice "frame-limited" property: if the associate memory of a managed object should be released within the current frame with the decrement to its last reference count, its lifespan will not overlap with subroutine calls after the decrement. Such property is not obstructed by the code motion to achieve possible memory reuse. In other words, the timeliness of RC operations are garanteed.

== Performance of the Koka Language

With reuse analysis, Koka programming language can achieve C/C++-like performance for common functional workloads, even with intensive data structure operations. @koka-benchmark showcases the performance measurements in @perceus.

#figure(
  image("figures/bench-amd3600-nov-2020.png", height: 70%),
  caption: [Koka benchmark data from @perceus. The benchmark was run on AMD 3600XT.]
) <koka-benchmark>

With the drop-guided reuse analysis from @frame-limited, the performance is improved further in @koka-benchmark2.

#figure(
  image("figures/frame-limited.png"),
  caption: [Koka benchmark data updated with drop-guided reuse analysis from @frame-limited. The benchmark was run on AMD Ryzen 5950X.]
) <koka-benchmark2>

There are some other optimization techiniques involved in the benchmark. TRMC stands for "tail recursion modulo calculus", which provides Koka the capability to transform recursive functions in certain forms into tail recursion. FBIP stands for "functional but inplace", which is a new program paradigm proposed in @perceus. This paradigm encourages programmers to write functional programs in ways that are friendly to reuse analysis. For instance, updating and walking red-black trees can be implemented with zippers, which "linearly" hold the status of traversal and exposes more opportunities for inplace updates.

The key takeaway for these benchmark results is that reuse analysis empowers functional languages with competitive performance to imperative programming by implicitly allowing inplace updates. Such technique largely improves the performance under workloads that are conventionally considered hard for functional languages such as data structure maintainence.

This benchmark also breaks the popular belief that the overhead of RC makes it incapable of delivering excellent performance when memory management is frequent. On the contrary, by optimizing RC as the first-class operations, fast update paths can get rid of most of its overhead. 

== Direct Interpolation: A By-Product of RC Runtime

While RC-based approach provides a measure to achieve inplace mutability, programmers may still need to access imperative objects, especially when interacting with low-level system interfaces.

Typically, such mechanisms require special wrappers inside the functional languages. Haskell, for example, restricts most imperative operations inside IO or other specific Monads. 

FFIs (Foreign Function Interfaces) between C/C++-like languages and memory-managed languages typically require much programmer effort to make sure object pass across language boundaries in a way that conforms the memory models of both languages. Such code can be tedious to work with and the managed languages usually post some hard limitations to the allowed operations inside FFIs. JNI, for example, is not compatible with the usual thread model of C/C++ and allows only single-threaded execution @JNI.

A RC-managed environment allows a much more elegant way to achieve interpolation:

- First, RC runtime is simple. There is nothing other than a thin header containing the reference count added to the managed object. Functional language objects can be created outside its own environment without too much effort. Maintaining the reference count does not introduce much complication either. One can wrap such object inside a RAII-like class to automatically maintain the correct ownership.
- Second, exclusivity encoded in RC allows imperative languages to modify the object efficiently without breaking the assumptions of functional languages. To modify an object, the imperative language can check and ensure the exclusivity by cloning the object if needed (similar to `Rc::make_mut` in Rust). Although the cloning operation can be costly, it is typically not invoked during the fastpath. 

For example, in the following code, one can implement the usual dynamic vector pushing operation on a RC managed object in Rust and then call such function inside functional language in normal ways. Promising simplicity can be achieved on both sides.
```rust
pub extern "C" fn vec_push(mut v : Rc<Vec<T>>, t: T) : Rc<Vec<T>> {
  v.make_mut().push(t);
  v
}
```

```haskell
// In an imaginary functional language
collect : u32 -> Vec<u32> -> Vec<u32>
collect 0 acc = acc
collect x acc = collect (x - 1) (vec_push acc x)
```

== Summary

In this chapter, we have examined how inplace mutation is achieved via RC-oriented optimizations. Contrary to traditional belief that RC can introduce maintainence overhead over fastpaths, @ullrich2020countingimmutablebeansreference @perceus @frame-limited iterate the reuse analysis algorithm and achieve no extra cost other than the necessary check of exclusivity. With @frame-limited, reuse analysis can even be used to avoid nondeterministic heap growth that can happen in both functional and imperative environment. Compared with previous solutions to the "Aggregate Update Problem", the RC-based solution is beneficical for both large and small objects and does not require manual selection of fixed operations. Compared with other performant GC methods, RC has a minimal runtime requirement which enables a shameless way to interpolate between functional and imperative world.

= Related Works <related-works>
In the previous chapter, we have seen RC-based memory reuse approaches with combined static analysis and runtime support. In this chapter, we hope to enhance the understanding of these approaches by covering more related works and studying their advantages and limitations over the RC-based approach.

== Linear Types can Change the World
In programming languages, linearity typically refer to the properties that a data of certain type can and must be used once @LinearUnique. The destructive move semantic in Rust mimics the affine linearity in the sense that an object is either moved (used) or dropped#footnote("Cloning an object creates a new object without consuming the original one. Both the original object and the newly cloned ones still conform the affine linearity.").

If a type is strictly linear, its data naturally exhibits exclusivity. Consider the case of wrapping dynamic vector in functional languages. One possible solution is to always mark the vector as linear, such that all updates can happen inplace. This idea leads to the linear types and uniqueness type systems from @LinearUnique @linear. In fact, "linear types can change the world" is the title of @linear. Indeed, linear objects can be mutated internally without breaking the referential transparency; and it is a "world-changing" strategy allowing programmers to introduce a form of "mutability" into functional world.

Different from RC-based approaches, the exclusivity for linear types is encoded directly in type systems and demands no runtime checking. In the situation where the programmer has a clear understanding that certain data is to be constructed linearly, this approach can achieve memory reuse with zero penalty.

The downside of the approach is more on ergonomics. Linear types and normal types are of different "colors"#footnote("We use the analogy of function coloring problems. Such problems also arise with asynchronous programming."). Once painted linear, it is hard to get the data back to normal color. With RC, such transitions are much more smoother: programmers do not even need to work with different types with different colors.

Haskell introduces linearity into its type system since GHC 9.10. @linear-haskell provides a detailed discussion on the design of the linearity in Haskell, including optimizations and ergonomics. For example, they use a notion of "linear arrows" to partially allow reusing code pieces with different colors. 

== Bufferization in MLIR
There are many situations where allocations and deallocations can be treated as internal operations of the language construction. For example, since C++20 @cpp, the standards allow C++ to rearrange, group and cancel `new` and `delete` operations. On one hand, this allows "constant evaluation" to get rid of memory operations. On the other hand, this opens opportunities for further optimizations.

Another important example in practice is the bufferization pipeline inside MLIR @Bufferization @MLIR. MLIR stands for Multi-Level Intermediate Representation. It allows users to compose different dialects in a unified IR framework and utilize existing facilities from those dialects together. `linalg` is one of the dialects, which provide functionalities to reason about linear algebra operations.

In machine learning frameworks utilizing MLIR (Tensorflow @tensorflow2015-whitepaper, PyTorch #footnote("Eager evaluation was used by PyTorch in its initial design, but the community is shifting their decisions towards a compiler-directed approach.") @paszke2019pytorchimperativestylehighperformance and JAX @jax2018github), linear algebra expressions are first composed  without evaluation. Then, a JIT/AOT compiler can kick in to optimize the "computation graph" in a holistic manner, eliminating unnecessary materialization of intermediate results. To achieve this, `linalg` operations inside MLIR are designed in "destination-passing style". That is, a `linalg` operation not only takes the input tensors but also the output buffers:

```asm
%c = tensor.empty (%dimX, %dimY : index, index) : tensor<?x?xf32>
%0 = linalg.matmul
    ins(%a, %b: tensor<?x?xf32>, tensor<?x?xf32)
    outs(%c: tensor<?x?xf32>) -> tensor<?x?xf32>
bufferization.materialize_in_destination %0 in restrict writable %dest : (tensor<?x?xf32>, memref<?x?xf32>) -> ()
```

The output buffer can be either constructed from `tensor.empty` or passed into the function as existing external buffers. `linalg` operations are subject to further optimizations such as elementwise operation fusion. After these optimizations, intermediate results buffered in empty tensors may no longer be needed. An empty buffer elimination pass can be used to remove such empty tensors.

There are two situations for the empty buffer removal:
- The empty buffer is no longer referred by any computation. This is usually resulted from operation fusion.
- The output buffer of the computation is explicitly materialized in a destination that is available at the time of computation. As such, the destination memory resource can be directly utilized without allocating any new buffer.

Compared with the RC-based reuse analysis, the bufferization pipeline is also reasoning about the passing of memory resource. However,
1. bufferization is more focused on the temporary buffers created around the computation#footnote("There are options to make bufferization pipeline go beyond the function boundaries but they do not change the affect that the fact that empty buffer elimination only focus on locally created buffers."). Reuse analysis, on the contrary, is based on analyzing potential exclusivity that may not be locally decided within current call frame.
2. bufferization mainly operates on allocations and deallocations. The optimizations does not involve the maintainence of reference count.
3. bufferization eliminates allocations if the associated buffer is materialized explicitly to memory locations that is available at the time of computation while the frame-limited reuse analysis is about paring memory tokens available within the context to allocation sites.

= Future Works <future-works>
Reuse analysis is developed with the persuit of performance and simplicity. However, RC-based approach is not without its limitations. In this chatper, we take a brief review of remaining issues. We also propose possible solutions in a way that can be applied together in a unified framework.

== Cyclic References and Local Mutability

A major issue of RC is that it cannot reclaim memory blocks with cyclic references. In @mutual-reference, both $A$ and $B$ are holding references to the other object. As a result, their reference count can never be deducted to zero, leading to memory leakage.

#figure(
  image("figures/cyclic.png", width: 35%),
  caption: [Mutually referenced objects.]
) <mutual-reference>

It is worth noting that this is usually not a problem in functional languages. Functional data structures are inductively or co-inductively defined @Pfenning2018. Without mutability, there is no possibility to form cycles among functional objects. However, functional languages are not always pure, OCaml, Scala, Lisp and many other common implementations all provide mutable references locally or even globally.

Project Verona @Verona @parkinson2024reference is another project utilizing RC-managed objects in its language runtime. Project Verona studies scheduling tasks safely and efficiently with concurrency. As such, the language design contains mutable and immutable regions @parkinson2024reference. When a RC-managed object escapes from a locally mutable area, a "freeze" operation will be applied and make the associate object immutable. As cycles may form inside the mutable region, the freezing procedure is designed to collect objects into multiple SCCs (Strongly Connected Components). While the reference count is recorded in the designated representative object, all other objects in the SCC tracks their path to the representative in a union-find manner. This information can be maintained by pointer tagging hence implies negligible overhead#footnote[To track the allocated objects in a region, one can maitain a singly-linked list with another extra word inside the object header.].

We hope to practice this idea 

// = High-level Memory Reuse Runtime

// == Reference Counting Runtime without Type Erasure <no-erasure>
// Two representative languages implementations that utilize Rc-based reuse analysis are Koka @perceus and Lean @lean-4. Albiet the differences in how they approach the reuse analysis, their runtime shares various common features:

// 1. Both languages are lowered to C or LLVM-IR directly obtained from C code, where memory management is performed with low-level pointer arithmetics.
// 2. Type are erased. Runtime objects are stored in a uniform way. The runtime does not use the type info to allocate or deallocate objects. Instead, basic information such as number of fields are simply stored in an extra header field associated with the object. Runtime replies on fields scanning to accomplish recursive destruction. 
// 3. Boxing and unboxing are heavily involed. To manage all language objects in a uniform way, scalar types are either tagged or boxed. This introduces extra indirections and branches during execution.

// We propose a higher level runtime implementation targeting the Rust programming language. Instead of erasing types during early stage of the IR manipulation, we hope to investigate the possibility to use higher-level language features to directly manage guest language objects. By implementing such runtime, we also hope to investigate some of the open questions surrounding reuse analysis.

// The frontend of a guest language can generate IR to provide function and inductive type definitions. Our framework will perform a series of operations to deliver final output in Rust. As one can see from the demo below, the IR itself can contain meta variables, which makes the frontend translation easier. As a side note, one should also notice that CFG on this IR is of tree structures, as there is no loop in functional programming. When lowering the code to Rust, however, the backend can create loops with tail-call optimization pass.

// #text(size: 10pt)[
// ```rust
// module test {
//     enum List<T>
//         where @T: Foo
//     { Nil, Cons(@T, List<@T>) }
//     fn test<T>(%1: i32, %2: f64) -> i32 
//       where @T: std::TraitFoo + std::TraitBar<Head = ()> 
//     {
//         %3 = constant 3 : i32;
//         return %3;
//     }
//     fn test2<T : Ord>(%0 : @T, %1 : @T) -> List<@T> {
//         %2 = %0 < %1;
//         %3 = new List::Nil;
//         if %2 {
//           %4 = new List::Cons, %0, %3
//           return %4;
//         } else {
//           %5 = new List::Cons, %0, %1;
//           return %5;
//         }
//     }
// }
// ```
// ]

// We plan to implement the reuse analysis and code lowering in a multi-pass manner. Some key steps are listed as the following:

// 1. *type inference*: Infer types for operands. This pass also checks that function calls and basic operations are well-formed.
// 2. *liveness analysis*: Detect the last-use frontier for operands. After this pass, explicit `drop()` operations will be inserted.
// 3. *clone analysis*: Check multiple uses on a single managed object and insert `clone()` accordingly.
// 4. *shape analysis*: Analyze the memory layout of managed objects.
// 5. *reuse pairing*: Decide proper memory reuse and pair reuse tokens with allocations.
// 6. *specialization and other optimizations*: Other optimizations such as in-place updates and tail-call recursion.

// In the following sections, we highlights the problems we want to investigate with our proposed framework and some proposed solutions.

// == Encoding Memory Reuse with Affline Linearity

// Rust language has a unique ownership design to garantee its memory safety. Such ownership system implicity provides similar programming model such as linear types and affine types @rust-affine. Researchers have shown the powerfulness of Rust's ownership system by developping explicit permission separation utilizing invarant lifetime markers as branded access tokens @ghost-cell.

// While the powerful ownership checking mechanisms significantly improve memory safety, it makes Rust unfriendly to be a codegen target for guest languages, as the code generator would have extra burden to garantee ownership and lifetime is handled properly.

// We noticed similarity between the requirement of Rust and the Rc-based memory reuse runtime. As discussed above, to achieve memory reuse, ownership of the Rc-managed object is transferred to the callee on each function call. If there is any pending use of the same argument object after the functional call, an explicit `clone()` must be inserted to garantee the liveness of the object. Since Rust defaults to move semantic and has a strict ownership checker, it offers free correctness assurance for the generated code. As one will see in the following sections, we will also allow object borrows in foreign language interface (FFI). Rust will also protect us from memory errors in such cases.

// == Seamless Interpolation <interpolation>

// Let's take a look at a "side effect" of Rc-based reuse analysis. Linearity provides a way to hide side effects in functional programming @linear. In the context of an Rc-based runtime, this approach enables direct manipulation of imperative data structures with minimal additional abstraction. This is illustrated with the following examples:
// ```rust
// // In Rust
// extern "rust" fn Vec::push<T>(mut v : Rc<Vec<T>>, t: T) : Rc<Vec<T>> {
//   v.make_mut().push(t);
//   v
// }
// // In an imaginary functional language
// collect : u32 -> Vec<u32> -> Vec<u32>
// collect 0 acc = acc
// collect x acc = collect (x - 1) (Vec::push acc x)
// ```
// Notice that in the guest language, although the `Vec<i32>` is managed by `Rc` down to the low-level runtime, we do not explicit mark it in the high-level syntax. The `Rc::make_mut()` function is an existing function in Rust's `alloc` library, which is roughly doing the following operation:

// ```rust 
// impl<T : Clone + ?Sized> Rc<T> {
//   fn make_mut(&mut self) -> &mut T {
//     if Rc::strong_count(self) != 1 || Rc::weak_count(self) != 0 {
//       // clone if the Rc pointer is not exclusive
//       *self = Rc::new(self.clone());
//     } 
//     // obtain a mutable referece in place
//     unsafe { &mut *(&*self as *const _ as *mut _)  }
//   }
// }
// ```
// Assume we are constructing the initial data structure using `collect 10000 Vec::new`. One should notice that in this fast path, there is no `clone()` needed as the pointer to the object is always exclusive. In this way, `Rc-based` reuse analysis, provides a canonical way to convert a clonable#footnote[We assume that `clone` operation garantees that the original object and the new object should function observably in the same way.] imperative data structure to functional programming language without loosing mutability. 

// With Koka and Lean, this method can be used to encapsulate common data structures like dynamic array and hash tables. While being efficient, there are several caveats related to this particular method. The data structure pointer is passed across FFI, and it is up to the user to maintain the correct reference counting operations, which can be tedious and error-prone. 

// We hope to investigate the possibility of direct interpolation with host language. When compiling the Rust, we hope to provide a way to automatically generate necessary wrappers for mutable imperative data structures. Given proper trait bounds, such wrappers will also rule out invalid operations.

// As suggested in @fp2, with Rc-based memory reuse runtime, functions (especially FFIs) may desire borrowed refereces for better performance. For instance, if we pass Rc pointers into `length : String -> usize`, the function will have to hold the ownership and generate code related to clean up the string. A better way is to mark the `String` as passed-by-reference, which would end up with a much cleaner code. However, in Koka and Lean's FFI, every object is passed as a pointer without any safety garantee. A carelessly implemented host language function may modify the reference counter or mutate the data fields accidentally and leads to unexpected consequences. 

// As readers will see in @uniqueness, borrowing the idea from @Kappa, @Pony and @Verona, we have proposed a fine grained division of argument passing styles with different reference capabilities. Such division works closely with Rust's type system, assuring the correctness across host and guest languages.

// == Efficient HOAS

// High Order Abstract Syntax (HOAS), as discussed in @hoas, is a methodology to represent guest language structure directly using high-level constructions in host language. For example, instead of using traditional AST to represent the lambda expression, the guest lambda can be directly encoded as a host lambda when feasible. 

// Lambda expressions are indeed one of the most complicated runtime features. As first-class construction in functional programming, lambda expression demands sophisticated runtime support, such as variable capture and partial application. In Lean4, the runtime has a special `lean_closure_object` consisting of plain function pointers and additional heap space for captured objects. Since object's type is erased upon capture, Lean4 created a "uniform ABI" where all values used by the lambda are boxed, including managed objects and scalar values. This creates two further complications. First, an indirection layer for boxed function call must be created for normal functions, as they may also be used as lambdas. Second, passing scalars may result in counter-intuitive overheads including tagging and memory allocation.

// ```c
// typedef struct {
//     lean_object   m_header;
//     void *        m_fun;
//     uint16_t      m_arity;     
//     uint16_t      m_num_fixed;
//     lean_object * m_objs[0];
// } lean_closure_object;
// ```

// We have investigated the possibility of using Rust's closure to directly encode guest's lambda expression. With this setting, Rc-managed objects and scalar values can be directly captured into `Rc<dyn BoxedClosure<P, R>>` object, where `P` is a list of parameters and `R` is the return type. The trait bound of `BoxedClosure<P, R>` is implemented for `Thunk`. The dynamic trait object is desired here. One can associate wrappers around plain functions or closures with the trait bound. In either way, it can be passed or stored uniformly as an object.

// ```rs
// pub struct Thunk<F, P: PartialParams, R>
// where
//     F: FnOnce(P::Full) -> R + Clone,
// {
//     code: F,
//     params: P,
// }
// ```
// As shown in the code above, a `Thunk` is just a Rust `FnOnce` object (it can either be a closure or a plain function closure, but must be decided statically), associated with a tuple of parameters representing the partial application status. 


// ```rs
// impl<T0: Clone, T1: Clone, T2: Clone, T3: Clone> PartialParams
// for (Ready<T0>, Ready<T1>, Ready<T2>, Hole<T3>) {
//     type Pending = (T3,);
//     type Progress = (Ready<T0>, Ready<T1>, Ready<T2>, Ready<T3>);
// }
// ```

// Elements inside the parameter tuple is tagged with either `Ready` or `Hole` to represent whether the positional argument is supplied or not. The associated type `Progress` denotes the next state to transit when a further argument is supplied. One should notice that this design fits the Rc-based reuse analysis properly: when an additional argument is supplied, the closure will check if it holds the exclusive reference to the underlying `Thunk`. If the reference is unique, it either updates he parameter pack in place or consumes the `code` field direcly, depending on whether this `params` are full. Otherwise, `clone()` operation will be performed. Such `clone()` is shallow since values are captured as `Rc`-managed objects or scalars --- there is no need for recursive `clone()`.

// Guest language closures can be directly translated to clsoure syntax in IR, which,in turn, can be translated to Host languages' closures.

// ```rust
// module test {
//   fn test() -> fn (i32, i32) -> i32 {
//     %0 = constant 991208 : i32;
//     %1 = (%2 : i32, %3 : i32) -> i32 {
//       %4 = %2 + %3;
//       %5 = %4 + %0;
//       return %5;
//     };
//     return %1;
//   }
// }
// ```

// We have experimented the proposed implementation with sample codegen results. Tests are passed under Rust's mid-level IR interpreter @Miri. There is no undefined behavior or other memory errors. 
 
// == Uniqueness Type System without Isolation <uniqueness>

// The following Lean4 code adds $1.0$ to all elements inside a `FloatArray` that stores elements in a fixed size contiguous memory area. 

// ```lean
// partial def add1(x : FloatArray) : FloatArray :=
//   let rec loop (r : FloatArray) (i : Nat) : FloatArray :=
//     if h : i < r.size then
//       let idx : Fin r.size := ⟨ i, h ⟩
//       loop (r.set idx (r.get idx + 1.0)) (i+1)
//     else
//       r
//   loop x 0
// ```

// Notice that we have carefully crafted the code such that there is no boundary checking. Ideally, with trivial tail-call optimization, the above code should be transformed into a loop feasible for SIMD vectorization. Indeed, `leanc` will transform the code above into the following `C` code where updates are happening in loop.

// ```c
// LEAN_EXPORT lean_object *l_add1_loop(lean_object *x_1, lean_object *x_2) {
// _start: {
//   lean_object *x_3;
//   uint8_t x_4;
//   x_3 = lean_float_array_size(x_1);
//   x_4 = lean_nat_dec_lt(x_2, x_3);
//   lean_dec(x_3);
//   if (x_4 == 0) {
//     lean_dec(x_2);
//     return x_1;
//   } else {
//     double x_5;
//     double x_6;
//     double x_7;
//     lean_object *x_8;
//     lean_object *x_9;
//     lean_object *x_10;
//     x_5 = lean_float_array_fget(x_1, x_2);
//     x_6 = l_add1_loop___closed__1;
//     x_7 = lean_float_add(x_5, x_6);
//     x_8 = lean_float_array_fset(x_1, x_2, x_7);
//     x_9 = lean_unsigned_to_nat(1u);
//     x_10 = lean_nat_add(x_2, x_9);
//     lean_dec(x_2);
//     x_1 = x_8;
//     x_2 = x_10;
//     goto _start;
//   }
// }
// }
// ```

// However, there are several issues stop `add1` from being vectorized. The first problem is that, along the loop, there are two `lean_dec` operation. This is because loop index variables can be potentially boxed in Lean. Such operations can be eliminated with our framework, since we will actively avoid boxing and unboxing as described in @no-erasure.

// The other problem is less trivial and harder to resolve. `lean_float_array_fset` and `lean_float_array_fget` are mutation operations on imperative data structures. From @interpolation, we have learned such operations incur a check on exclusivity and a potential slow path that clones the underlying data. Vectorizer cannot conduct optimizations due to the existence of these cold routines.

// A straightforward solution is to have uniqueness in type system @Uniqueness @LinearUnique. With proper annotations from users, the compiler can statically determine the uniqueness (or exclusivity) of objects. Thus, extraneous checks on uniqueness can be removed together with slow paths. Such type systems, however, usually suffer from the "coloring" problem. Once a function is colored as requiring uniqueness or linearity on its input, it acquires substantial efforts to passing data between "colored" worlds and "uncolored" worlds. @fp2, on the other hand, provides special annotations that let compilers assist users to check the correctness of their uniqueness markers, thus garantees that in-place updates are indeed performed as expected. Such approach, however, does not solve the "coloring" problem.

// We propose a more friendly approach to incorporate static uniqueness into our runtime. We also assume that users are capable of specifying uniqueness requirements in performance critial routines. Notice that, our Rc-based runtime provides two operations: (1) checking the exclusivity of a Rc reference, and (2) obtaining shallow copies of a managed object. These two operations enable easy transitions between "colored" and "uncolored" functions. Upon calling a function that demands uniqueness, the compiler inserts a check on the uniqueness of the objects and use shallow copy to obtain a new exclusively owned object if necessary. In this way, one can lift the checks in loop out of the body, leaving a clean code path for further optimization opportunities. What's more, calling between "colored" and "uncolored" functions do not require explicit users' efforts.

// Together with the discussion in @interpolation, our framework supports three sorts of references together with the memory reuse token. Their conversions are detailed in @reference-diagram.

// #let ref-diagram = { 
//   let Rc = (0, -1)
//   let Unique = (0, 1)
//   let Ref = (1, 0)
//   let Token = (-1, 0)
//   fletcher.diagram(
//     node-stroke: 1pt,
//     edge-stroke: 1pt,
//     node(Rc, "Rc<T>"),
//     node(Unique, "Unique<T>"),
//     node(Ref, "&T"),
//     node(Token, "Token<T>"),
//     edge(Unique, Rc, "<|-", [uniquefy], label-pos: 0.9),
//     edge(Unique, Rc, "-|>", [direct cast], label-pos: 0.1),
//     edge(Rc, Ref, "-|>", bend: 30deg, [borrow]),
//     edge(Unique, Ref, "-|>", bend: -30deg, [borrow]),
//     edge(Token, Rc, "-|>", bend: 30deg, [reuse], label-pos: 0.1),
//     edge(Rc, Token, "-|>", bend: 30deg, [drop], label-pos: 0.7),
//     edge(Token, Unique, "-|>", bend: -30deg, [reuse], label-pos: 0.1),
//     edge(Unique, Token, "-|>", bend: -30deg, [drop], label-pos: 0.7),
//   )
// }
// #figure(
//   ref-diagram,
//   caption: [Reference Sorts]
// ) <reference-diagram>

// - `Rc<T>` is the traditional Rc pointer to a managed object.
// - `Unique<T>` can only be used in function parameters (not materializable as object fields). It is used to denote the exclusivity statically as discussed above. A possible runtime implementation is to add a wrapper to the `Rc<T>` with compiler instrinsics hinting the exclusivity (such as `llvm.assume` and `llvm.unreachable`).  
// - `&T` represents a borrowed reference to the object. As mentioned in @interpolation, such borrowed references can be used in FFI. When compiling to Rust, the reference is translated as a reference to the underlying value inside the `Rc` managed area. This setting automatically makes sure that safe FFI cannot manipulate the reference counting of the memory object, avoiding interference to the reuse analysis. 
// - `Token<T>` is the memory reuse token. The type parameter is needed to carry layout information, as needed in @open-type.

// == Open Type Parameters <open-type>

// #let basic-eq = curryst.rule(
//   label: [$PP_equiv$],
//   [$P_0 equiv P_1$],
//   [$ell(P_0) = ell(P_1)$],
// )
// #let basic-neq = curryst.rule(
//   label: [$PP_equiv.not$],
//   [$P_0 equiv.not P_1$],
//   [$ell(P_0) eq.not ell(P_1)$],
// )
// #let var-eq = curryst.rule(
//   label: [$VV_equiv$],
//   [$V_0 equiv V_0$],
// )
// #let var-sim = curryst.rule(
//   label: [$VV_tilde.equiv$],
//   [$V_0 tilde.equiv V_1$],
// )
// #let sum-equiv = curryst.rule(
//   label: [$plus_equiv$],
//   [$A_0 + B_0 equiv A_1 + B_1$],
//   [$A_0 equiv A_1$],
//   [$B_0 equiv B_1$],
// )
// #let sum-ne-l = curryst.rule(
//   label: [$plus_(equiv.not,L)$],
//   [$A_0 + B_0 equiv.not A_1 + B_1$],
//   [$A_0 equiv.not A_1$],
// )
// #let sum-ne-r = curryst.rule(
//   label: [$plus_(equiv.not,R)$],
//   [$A_0 + B_0 equiv.not A_1 + B_1$],
//   [$B_0 equiv.not B_1$],
// )
// #let sum-sim-l = curryst.rule(
//   label: [$plus_(tilde.equiv,L)$],
//   [$A_0 + B_0 tilde.equiv A_1 + B_1$],
//   [$A_0 tilde.equiv A_1$],
//   [$B_0 equiv B_1$],
// )
// #let sum-sim-r = curryst.rule(
//   label: [$plus_(tilde.equiv,R)$],
//   [$A_0 + B_0 tilde.equiv A_1 + B_1$],
//   [$A_0 equiv A_1$],
//   [$B_0 tilde.equiv B_1$],
// )
// #let sum-sim = curryst.rule(
//   label: [$plus_(tilde.equiv)$],
//   [$A_0 + B_0 tilde.equiv A_1 + B_1$],
//   [$A_0 tilde.equiv A_1$],
//   [$B_0 tilde.equiv B_1$],
// )
// #let prod-equiv = curryst.rule(
//   label: [$times_equiv$],
//   [$A_0 times B_0 equiv A_1 times B_1$],
//   [$A_0 equiv A_1$],
//   [$B_0 equiv B_1$],
// )
// #let prod-ne-l = curryst.rule(
//   label: [$times_(equiv.not,L)$],
//   [$A_0 times B_0 equiv.not A_1 times B_1$],
//   [$A_0 equiv.not A_1$],
// )
// #let prod-ne-r = curryst.rule(
//   label: [$times_(equiv.not,R)$],
//   [$A_0 times B_0 equiv.not A_1 times B_1$],
//   [$B_0 equiv.not B_1$],
// )
// #let prod-sim-l = curryst.rule(
//   label: [$times_(tilde.equiv,L)$],
//   [$A_0 times B_0 tilde.equiv A_1 times B_1$],
//   [$A_0 tilde.equiv A_1$],
//   [$B_0 equiv B_1$],
// )
// #let prod-sim-r = curryst.rule(
//   label: [$times_(tilde.equiv,R)$],
//   [$A_0 times B_0 tilde.equiv A_1 times B_1$],
//   [$A_0 equiv A_1$],
//   [$B_0 tilde.equiv B_1$],
// )
// #let prod-sim = curryst.rule(
//   label: [$times_(tilde.equiv)$],
//   [$A_0 times B_0 tilde.equiv A_1 times B_1$],
//   [$A_0 tilde.equiv A_1$],
//   [$B_0 tilde.equiv B_1$],
// )
// #let inference-rules = {
// let pt = curryst.proof-tree;
// $
// #pt(basic-eq)   #pt(basic-neq)  #pt(var-sim) #pt(var-eq)  \
// #pt(sum-equiv)  #pt(sum-ne-l)   #pt(sum-ne-r)  \ 
// #pt(sum-sim-l)  #pt(sum-sim-r)  #pt(sum-sim)   \
// #pt(prod-equiv) #pt(prod-ne-l)  #pt(prod-ne-r) \ 
// #pt(prod-sim-l) #pt(prod-sim-r) #pt(prod-sim)
// $
// }

// As proposed in @no-erasure, our IR supports meta variables. While meta variables provide a straightforward way to translate guest language polymorphism into host language polymorphism, it complicates the reuse analysis, especially in the shape analysis pass. 

// If type variable appears in a managed object, its memory layout may not be decidable during static analysis. To accommodate meta variables, we introduce a novel algorithm to decide the feasibility for memory reuse.

// Assume types are inductively defined using primitive types $P_i in PP$, meta variables $V_i in VV$, together with product $A times B$ and sum $A + B$ operations. We say type $A$ and $B$ are of the equivalent shape ($A equiv B$) if both $A$ and $B$ are closed types without any meta variable, which means both $A$ and $B$ are of statically decidable memory layouts and their layouts are the same. Otherwise, if any component decidably differ in memory layout, the composite types in comparison (or primitive types if there is only a single component) do not have equivalent shapes ($A equiv.not B$). However, when meta variables are involved, we additionally say that $A$ and $B$ are of similar shapes ($A tilde.equiv B$) if their shapes are equivalent with compatible meta variable substitution. Similar shapes also imply possibility for memory reuse. 

// @inference-rules summarizes the inference rules for deciding shape equivalence. It's noteworthy that for sum types, Rust can employ advanced layout optimizations to devise the most compact and efficient memory layout possible. The nuances such as the number and order of operands can be relevant when deciding the layout. Therefore, for both sum and product types, the equivalence checking algorithm behaves similarly to "alpha-equivalence" (structurely identical except for meta variables) checking.  There may be more fine-grained ways to decide the shape equivalence of types, potentially leading to more reuse opportunities. For now, we leave such algorithms for future study. 

// #figure(
// text(size: 9pt, inference-rules), caption: [Inference Rules for Shape Analysis ($ell$ is the static memory layout function)]) <inference-rules>

// For types with similar shapes, the analysis pass cannot direcly decide reusability. Hence, we will need to emit Rust code to check the compatibility of memory layout. One can optimistically generate reuse token, but drop the token and allocate appropriate memory area if layout does not match. Fortunately, such checks are static with respect to Rust's compiler. By putting up checks properly, there can be no overhead at all.

// == Memory Reuse Fusion and Specialization

// Another complication for Rc-based high-level memory reuse runtime arises from abstraction of Rc pointers. We have illustrated that both Koka and Lean make memory reuse possible by internalizing Rc operations as intrinsics in IR. In our framework, we also use such intrinsics in IR. When lowering into Rust, however, to achieve seamless interpolation as promised in @interpolation, managed objects are simply materialized as normal language objects (`Rc<T>`). 

// We want to highlight that by providing enough facilities around `Rc<T>`, one can embed the memory reuse operations into the host language. For instance, when mutate imperative data structures, `Rc::make_mut()` is used to garantee exclusive access to objects. Besides, one can also utilize `drop()`, `clone()` provided by Rust itself, together with `drop_for_reuse()`, `Rc::reuse_or_alloc()` provided by our framework to achieve memory management.

// Optimal codegen requires specializations. For instance, one can identify field update operations and skip over all the elimination and introduction procedures. 

// Specifically, we demonstrate a special case to fuse memory management operations. Recall the conceptual code obtained by inlining destruction procedures:

// ```rs
// List::Cons(ref y, ref ys) => {
//   let y = y.clone();
//   let ys = ys.clone();
//   let mem = if Rc::is_unique(xs) {
//     drop(y);
//     drop(ys);
//     Some(Rc::take_memory(xs))
//   } else { None };
//   reverse(ys, Rc::reuse_or_alloc(mem, List::Cons(y, acc)))
// }
// ```
// The fusion step will push down the `clone()` operations and cancel them with `drop()` on the fast path. However, this is not direcly approachable in rust, since later on, when constructing `List::Cons(y, acc)`, we still need to value bounded to `y`. There is no easy way to remove extra cost due to `clone()`.

// Fortunately, Rust provides `Rc::unwrap()` operation. Addtionally, we can provides `Rc::unwrap_for_reuse()`. With exclusive access, such functions simply move the values out of the Rc-managed memory area and generate reuse tokens. Otherwise, they clone the values. In either way, one will get a value rather than Rc reference for the underlying object.

// ```rust
// List::Cons(..) => {
//   let (token, List::Cons(y, ys)) 
//     = Rc::unwrap_for_reuse(xs) else { core::hint::unreachable() };
//   reverse(ys, Rc::reuse_or_alloc(mem, List::Cons(y, acc)))
// }
// ```

// Such operations provide a one-step fusion for memory reuse on the fast path.

// = Open Problems

// In this section, we will discuss aspects that are not studied in this work but remain interesting for future exploration. 

// == Dataflow Formulation <dataflow-formulation>

// For functional programming languages, typical control flow graphs are directed trees. In our framework, the ways to form branches are also limited to elimination rules (including the if-then-else branch for booleans).

// In general settings, the CFG can be much more complicated. One way to proceed is to employ control flow analysis (CFA) @cf-analysis, where information associated with code points are organized in lattices and special join/meet operations are developed to handle multiple in-degrees and loops. 

// Reuse analysis resembles CFA in the sense that a drop operating creates an available memory token. Such tokens are carried in context until being consumed at an allocation site. 

// The dataflow formulation is interesting for future research as it can extend our proposed framework to support imperative programming languages where Rc objects are internalized in IR.

// == Optimal Pairing for Memory Reuse 

// As discussed in @copy-avoidance, if there are multiple memory tokens and allocation sites in context, deciding optimal memory reuse pairing to avoid copy is a NP-Hard problem. 

// Koka and Lean utilize simple heuristics such as preference to reuse tokens from same types. These heuristics will typically lead to good enough choices. However, we think it is of academic value to investigate constained memory reuse problem. As mentioned in @frame-limited, frame-limitation is an important property that we want to preserve during pairing memory tokens, since it rules out nondeterministic heap growth. It is noteworthy that frame-limitation cut off possible reuse choices, as memory tokens should not propogate long distances. Hence, one can explore the possibility to have efficient algorithms that decide optimal memory token assignment under frame-limited settings.

// @dataflow-formulation proposed general reuse analysis as a dataflow problem. If we can to achieve optimality with CFA, the cost model of reuse token assignments must be carefully designed such that the model can converge with iterative evaluation.

// Memory reuse is an optimistic optimization that speeds up the fast path. Without doubt, such optimizations incur extra costs on slow routines. @open-type described the open type problems, which adds more probabilistic ingredients into the problem. As such, an optimal memory reuse decision should consider the gap between optimistic expectation and realistic execution. Profile-guided optimization can be an interesting approach for future exploration.

// == GC Integration

// Rc-based memory management policy suffers from cyclic reference problem, where objects organized in cycles are permanently leaked. It is not a problem for purely functional programming languages with only (co-)inductively defined data types. However, special local and global mutable reference are also widely adopted in various functional languages, that can potentially lead to cycles.

// The sites that possibly form cycles can be identified with careful design. However, it remains an open problem to find good choices for handling cycles. When using functional programming for concurrency, purely Rc-based approach may not be desirable due to considerable overhead from atomic operations. All these issues suggest that one might investigate integrating GC and Rc together and conditonally switching between them. Some pioneering works are done in @NIM @ORCA. 

// Rc has already been heavily employed in modern garbage collectors, as in @lxr @rc-immix. These approaches usually utilize very small referece counters and once an object is upgraded to GC management due to overflow, the ownership and exclusivity information is permanently lost. Thus, a proper way to preserve information for memory reuse remains to be discovered.

// == Value Sharing Problem

// Consider the following code in Koka:
// #text(size: 10pt)[
// ```js 
// fun subst(t : tree<a>, x : int, y: int) : tree<a>
//   match t 
//     Node(l, v, r) ->
//       if v == x then Node(subst(l, x, y), y, subst(l, x, y))
//                 else Node(subst(l, x, y), v, subst(l, x, y))
//     Leaf -> t
// ```
// ]

// When substituting an non-exist value, `subst` is actually a fixed-point operation. Ideally, no allocation should happen since the tree should no be changed at all. Indeed, if the tree is uniquely owned, the reuse analysis will effectively doing inplace updates that end up doing nothing. However, if the tree is shared, extra allocation is unavoidable with only reuse analysis.

// A possible solution is as the following:

// 1. Apply interprocedual analysis to mark functions that potentially acts as a fixed point with repect to its input arguments.

// 2. When returning from such function and constructing a value that potentially "overlaps" with the original value, check the addresses of return values. If addresses stay the same, propogate the value sharing by return the original object.

// With memory reuse, such process is actually problematic, as inplace updates will give back the same addresses with distinct values. However, it is possible to apply value sharing optimization on slow paths where we know target value is shared and no inplace updates is possible. Another complication is tail-call optimization as it changes the convention of argument passing. Therefore, one should design a cost model carefully to decide value sharing strategies.

// = Conclusion


// We have delved into the essence of memory reuse by examining the fundamental rules that underpin computations in functional programming. Bearing this essence in mind, this work introduces a novel approach that compiles Rc-based memory reuse from guest languages to high-level languages, such as Rust, with an emphasis on robustness and efficiency. Throughout our design process, we encountered several challenges, including the avoidance of type erasure, navigating the move semantics of the host language, compiling with higher-order abstract syntax (HOAS), handling interpolation, ensuring uniqueness typing, addressing open type problems, and implementing specializations with reuse fusion.

// Additionally, we have identified open problems related to dataflow cost modeling, the integration of garbage collection (GC) mechanisms and the value sharing problem. As proof assistants and other related applications continue to evolve, the demand for increasingly sophisticated functional programming capabilities grows. We believe that our framework holds significant value for both the academic and industrial sectors, offering a potent tool for enhancing the efficiency and effectiveness of functional programming languages in managing memory reuse.
