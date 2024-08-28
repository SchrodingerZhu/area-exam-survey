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

This survey tries to bring together two seemingly exclusive concepts: *inplace mutations* and *immutable objects*. Immutable objects have been introduced into various languages due to their inherent memory safety and friendliness to static analyses @rust-affine @ullrich2020countingimmutablebeansreference @Verona. Functional languages, for example, can utilize the referential transparency implied by immutability to avoid repetitious evaluation. Rust and other memory-safe languages apply immutability to shared and concurrent objects to ensure soundness of memory operations.

Despite its elegance, immutability appears to be a double-edged sword. As for functional programming, many data structures and algorithms are complicated to implement in functional paradigm. Some may even be impossible to maintain its original efficiency when written in a purely functional style devoid of any inplace mutation. Performance for general situations may also be affected as it is no longer possible to apply inplace updates. Objects often need to be frequently constructed or destroyed in whole even though a state change only touchs partial fields.

This survey wants to explore solutions to these problems. By studying exsiting works, we hope to answer two big questions regarding immutable objects:

1. *Is it possible to achieve inplace mutability without breaking the "immutable views" of objects?* If this is possible, what efforts are needed for runtime and static analysis?

2. *Is it possible to interpolate objects from both mutable and immutable worlds?* Does such interpolation require extra efforts from programmers?

In order to answer these questions, we arrange the survey in the following structure:

- @functional-programming will introduce some basic backgrounds of functional programming and common design patterns on immutable objects. We focus on functional programming as immutability is heavily employed in such paradigm. We will be able to see when and why immutability is causing problems.

- @early-story will summarize traditional methods to handle such immutability issues. This chapter mainly includes two aspects: 1) the definition of *large object* and *aggregate update* problems in functional programming and their solutions; 2) how sophisticated memory managment algorithms mitigate performance panelty due to frequent object constructions and destructions due to immutability.

- @RC-Revisit continues on exploring the solutions but focusing on some newly developed methods based on *reuse analysis*. This chapter will discuss runtime facilities and static analysis required by efficient memory reuse. We will not only see that RC-based approaches break the common belief on the heavy overhead of RC maintainance but also see how RC operations themselves provide precise cut-in points for static analysis. Besides solving the inplace update problem, it also explains why reference counting (RC) based runtime enables a straightforward way to wrap imperative data structures into functional programs.

- @related-works summarizes similar works related to reuse analysis in @RC-Revisit. This chapter includes both high-level type system design and similar analyses outside reference-counted system. Issues surrounding immutable objects can be further generalized to any singly-assigned values that demand certain resources. For instance, it is common for machine learning frameworks to compose tensor expressions. The buffers holding intermediate tensors must be appropriately optimized to avoid premature materialization. We will see how MLIR's bufferization pipeline eliminates and fuses temporary buffers.

- @future-works  will propose potential improvements to the reuse analysis and provide a detailed discussion on implementation difficulties. There are still many missing pieces in existing works, such as how to minimize the impact of the reference count checking on fastpaths, the codesign of mutable and immutable regions, and the locality and security implication of memory reuse. We hope to offer a glance at these opportunities in this chapter. 

= Functional Programming <functional-programming>

Functional programming is typically associated with a paradigm that formulates programs as lambda expressions and views computation as the reduction or normalization of lambda terms. In a purely functional framework, evaluations are devoid of side effects, allowing programs to be regarded as "functions" in the mathematical sense @pragmatics. This approach to programming simplifies the handling of complex problems: values are inherently persistent and sharable @advanced-data-structures @optimal, immutability prevents data races in concurrent programming, and lambda calculus embodies the core of constructive proofs according to @proofs-as-programs.

== State Transitions in Functional Programming
Being immutable, however, functional paradigm has its negative implications to performance. Consider the State Monad implementation in Haskell from MTL @mtl @state-monad. Without the capability to mutate individual field, the state transition is achieved by constructing new state objects and passing it to the desired continuation (the lambda expression wrapped in `state` constructors). If the code were not properly optimized, each of such state transitions is accompanied by deallocations of original state objects, allocations of new state objects, and necessary data copying. This will lead to considerable performance impacts especially when such objects are large enough. 

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

Consider the functional red-black tree balancing algorithm implemented by @algoxy. The implementation uses pattern matching syntax in Haskell. On the left-hand side, the function checks if the arguments conforming certain patterns. If a match is found, balanced nodes on the right-hand side are constructed as return values using matched values from left-hand side. Implicitly, such procedure indicates the arguments on the left-hand side can be destructed if there is no further reference to these values after execution. The right-hand side nodes are newly constructed, hence demand allocations. One can imagine that the functional implementation may lead to much more performance penalty due to the "immutable nature" of the paradigm.

== The Demands for Performance

Before we move on to the solutions, let's answer the following questions: 
1. Why does the performance of functional languages matter? 
2. To what extend does the immutability affects the overall efficiency?

In order to answer these questions, let's conduct an experiment on the Lean4  @lean-4 proof assistant. Lean4 and other dependently-typed functional programming languages are attracting increasingly more interests among cryptography, certified compilation and mathematics community. Compiler researchers and cryptography researchers are using them to verify critial programs @composable-verification @cryptoeprint. Mathematicians are embedding proof assistants into their workflows to tackle some most challenging works such as formalizing proofs to the Fermat's Last Theorem for regular primes @flt.

For proof assistants, their type systems are rather complicated, which usually require Bidirectional Type Checking @pi-forall @how-to-implement-ddt @how-to-code-your-own-type-theory and Normalization by Partial Evaluation (NbPE) @NbPE techniques on a relatively large ruleset. Many proof assistants choose to bootstrap themselves @lean-4 @agda. On one hand, the type checking rules can be closely represented in functional programming as the rules are typically described in typed lambda calculus. On the other hand, doing so provides a measure for these proof asisstants to assess their own correctness. The type checking algorithms mentioned above exhibits the common patterns of functional programming and thus affected by the performance implications. 

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

= Inplace Update in Functional Programming: An Early Story <early-story>

Unique properties including referential transparency has been attracting people from various communities to introduce functional programming into their research and production environments. As such, a primary challenge is to workaround the performance issues implied by immutability. In this chapter we focus on summarizing different approaches to tackle the so called "Aggregate Update Problem" (also known as "Large Object Problem") in functional programming environments.

== What is the "Aggregate Update Problem"?

Following the convention in @aggregate-problem, aggregate data types typically refers to data structures that collects an amount of elements in a dense, usually contiguous memory area, such as (multi-dimensional) arrays and strings. 

In an imperative environment, these data structures provide handy ways to store and access elements in an uniform manner with excellent locality. In the functional world, however, one cannot mutate the underlying memory objects (in user-visible ways). Otherwise, referential transparency can potentially be destroyed. Purely functional expressions are devoid of side effects. Many implementations utilize this fact to avoid repeated evaluations of the same expressions. If some memory objects referred by an expression are mutated in unexpected ways, the whole program may produce undesired results.

As shown in @functional-programming, to represent state transitions, functional programs carrying aggregate state objects as the arguments to their continuation. The most safe and conservative way to update aggregate objects is to clone them in whole with required change applied and then pass newly created objects to the continuation. If there are any evaluated expressions referring to old objects, their existing results can still be used as nothing is changed in their memory state.

```haskell
addOne : Array Int -> Int -> Array Int
addOne arr idx = 
  if idx >= len arr 
  then arr 
  else let elm = get arr idx in addOne (set arr idx (elm + 1)) (idx + 1)
```
For instance, in the above code, the builtin function `set` can be implemented to allocate a new array, copying the original array with the element at the provided index modified. The performance issue is immediately apparent. Increasing all elements in a linear array becomes a $O(n^2)$ operation with additional penalty on memory management.

== Solutions to the Aggregate Update Problem
Many solutions are proposed to workaround such issues. One mitigation is to use specially designed functional data structures. The intuition is that the "Aggregate Update Problem" only happens on "Large Objects". One can break down the data structure into small pieces thus localize the impact of modifications. Associated algorithms create updated objects with mutation happening on a small subset of the sharded pieces while reuse most unchanged parts that can be easily tracked via few pointers. Fingertrees, for example, are typically used as "arrays" in the functional world @algoxy @purely-functional-data-structures. One of their variants is adopted into the standard library of Scala with an amortized extra cost that is almost negligible under a wide range of workloads @scala-pr @rrb-vector.

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
Given a program composed with traditional IR operations (including projections, applications and constructor calls), the compiler can statically infer the required RC operations. Once explicit RC operations are obtained, more optimization opportunities are exposed to the compiler. For example, in the code above, the compiler notices that the memory associated with `xs` can possibly be reused. Hence, instead of allocating memory for constructors, a "destructive decrement" (`reset` operation) is inserted. A later on `reuse` operation can then avoid allocations if the memory pointer passed in is not null. 

The process proposed in @ullrich2020countingimmutablebeansreference involves three steps:
1. Inplace update operations including `reset` and `reuse` are inserted into feasible sites.
2. Some functions may not need to use RC pointers as they only require immutable projections or other trivial operations on their input arguments. Such functions can be converted to "borrow semantics" to avoid RC operations.
3. With destructive operations and borrowing operations inserted, it becomes straightforward to add necessary reference counting operations.

There are still other challenges such as how to tweak the functional data structures such as red-black trees to better fit into the inplace update scheme and how to efficiently maitain the reference counting in multi-threaded environments. These implementation details are discussed in @ullrich2020countingimmutablebeansreference. We skip them here to focus on the main topics.

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
In @ullrich2020countingimmutablebeansreference, the algorithms begins with inserting `reset` and `reuse`. With destructive move, the steps are reordered in @perceus. Given a spartan lambda algebra, one can first insert the `inc` and `dec` operations in the first step, assuming move semantics. Notice that the reference count can now be decreased as soon as its associated pointer is no longer used anywhere in the current frame.

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
The `inc` and `dec` operations can be inferred automatically. Notice that we consider pattern matching as a destructive operation, after which the decrement for the corresponding RC pointers should be inserted if the object is not used anymore. The above program will be annotated as the following:
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

Such decisions, however, may not be easy to make and can degrade performance if not handled carefully. Even if memory reuse is possible, the appropriate insertion site for reuse token creation may not be trivial to determine. Consider the following nested pattern matching from @frame-limited:

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
Since the compiler wants to enforce the memory reuse, it optimistically inserts an increment operation to allow passing `xs` to subroutine `f` while postponing the destructive decrement `reset` until the return of `f`, such that the constructor call can grab the chance of inplace memory reuse. The side effect of the code motion is that `xs` is kept alive during the whole lifetime of the subroutine call, disabling possible reuse inside the nested functions and possibly leading to nondeterministic heap growth in the worst situation!

All these issues show that looking ahead for constructor calls to decide whether or where a destructive decrement is to be installed may not be a favorable solution. This leads to the proposal from @frame-limited. 

It is suggested that the compiler should not distinguish the decrement in two different operations. Similar to the first step analysis in @perceus, `inc/dec` operations are inserted without the consideration of reuse. The `dec` operation denotes that some memory resource possibably becomes available within the context. We can do a backward dataflow analysis to populate possible allocations along the control flow. We carry the possibly available "resource" along the CFG, until it is paired with a suitable allocation at a contructor call or we no longer find any feasible allocation along the control flow. In the latter, we insert an explicit free operation of the memory resource. Another way to think about the algorithm is that we always insert a destructive decrement instead of normal `dec` and insert free if the produced memory token is not paired with any reuse operation.

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

There are some other optimization techiniques involved in the benchmark. TRMC stands for "tail recursion modulo calculus", which provides Koka the capability to transform recursive functions in certain forms into tail recursion. FBIP stands for "functional but inplace", which is a new program paradigm proposed in @perceus. This paradigm encourages programmers to write functional programs in ways that are friendly to reuse analysis. For instance, updating and walking red-black trees can be implemented with zippers, which "linearly" hold the status of traversal and expose more opportunities for inplace updates.

The key takeaway for these benchmark results is that reuse analysis empowers functional languages with competitive performance to imperative programming by implicitly allowing inplace updates. Such technique largely improves the performance under workloads that are conventionally considered hard for functional languages such as data structure maintainence.

These benchmark results also break the popular belief that the overhead of RC makes it incapable of delivering excellent performance when memory management is frequent. On the contrary, by optimizing RC as the first-class operations, fast update paths can get rid of most of its overhead. 

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
In the previous chapter, we have seen RC-based memory reuse approaches with combined static analysis and runtime support. In this chapter, we hope to enhance the understanding of these approaches by covering more related works and studying their advantages and limitations over the RC-based approach. There are two kinds of related works covered in this chapter:

1. We first discuss the linear type systems that explicity grant languages with the inplace mutation capabilities without static analysis. RC-based reuse analysis is related to this approach in the sense that even though the managed objects can be shared, the ownership is now passed linearly with move semantic.
2. We generalize the immutability problem to singly-assigned values and study how the MLIR's bufferization pipeline reuse existing buffers to avoid unnecessary materialization of tensor values.

== Linear Types can Change the World
In programming languages, linearity typically refer to the properties that a data of certain type can and must be used once @LinearUnique. The destructive move semantic in Rust mimics the affine linearity in the sense that an object is either moved (used) or dropped#footnote("Cloning an object creates a new object without consuming the original one. Both the original object and the newly cloned ones still conform the affine linearity.").

If a type is strictly linear, its data naturally exhibits exclusivity. Consider the case of wrapping dynamic vector in functional languages. One possible solution is to always mark the vector as linear, such that all updates can happen inplace. This idea leads to the linear types and uniqueness type systems from @LinearUnique @linear. In fact, "linear types can change the world" is the title of @linear. Indeed, linear objects can be mutated internally without breaking the referential transparency; and it is a "world-changing" strategy allowing programmers to introduce a form of "mutability" into functional world.

Different from RC-based approaches, the exclusivity for linear types is encoded directly in type systems and demands no runtime checking. In the situation where the programmer has a clear understanding that certain data is to be constructed linearly, this approach can achieve memory reuse with zero penalty.

The downside of the approach is more on ergonomics. Linear types and normal types are of different "colors"#footnote("We use the analogy of function coloring problems. Such problems also arise with asynchronous programming."). Once painted linear, it is hard to get the data back to normal color. With RC, such transitions are much more smoother: programmers do not even need to work with different types with different colors.

Haskell introduces linearity into its type system since GHC 9.10. @linear-haskell provides a detailed discussion on the design of the linearity in Haskell, including optimizations and ergonomics. For example, they use a notion of "linear arrows" to partially allow reusing code pieces with different colors. 

== Bufferization in MLIR
There are many situations where allocations and deallocations can be treated as internal operations of the language construction. For example, since C++14 @cpp, the standards allow C++ to rearrange, group and cancel `new` and `delete` operations. On one hand, this allows "constant evaluation" to get rid of memory operations. On the other hand, this opens opportunities for further optimizations.

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
1. bufferization is more focused on the temporary buffers created around the computation#footnote("There are options to make bufferization pipeline go beyond the function boundaries but they do not change the affect that the fact that empty buffer elimination only focus on locally created buffers."). Reuse analysis, on the contrary, is based on analyzing potential exclusivity that may not be locally decided within current call frame. The release of RC gives out the memory resource nondeterministically, where the buffer resource is deterministic in bufferization pipeline.

2. bufferization mainly operates on allocations and deallocations of tensors. The optimizations does not involve the maintainence of reference count, hence it is not using the liveness information as encoded in the `inc/dec` operations. It also involves no fusion on such operations.

3. bufferization eliminates allocations if the associated buffer is materialized explicitly to memory locations that is available at the time of computation while the frame-limited reuse analysis is about paring memory tokens available within the context to allocation sites. The former can obtain necessary information by looking at the destination buffer passed into the `linalg` operation. The latter requires more analysis along the control flow.

= Future Works <future-works>
Reuse analysis is developed with the persuit of performance and simplicity. However, RC-based approach is not without its limitations. In this chatper, we take a brief review of remaining issues and possible solutions. We hope to offer a glance at possible future development related to the inplace mutation of immutable objects. 

== Cyclic References and Local Mutability

A major issue of RC is that it cannot reclaim memory blocks with cyclic references. In @mutual-reference, both $A$ and $B$ are holding references to the other object. As a result, their reference count can never be deducted to zero, leading to memory leakage.

#figure(
  image("figures/cyclic.png", width: 35%),
  caption: [Mutually referenced objects.]
) <mutual-reference>

It is worth noting that this is usually not a problem in functional languages. Functional data structures are inductively or co-inductively defined @Pfenning2018. Without mutability, it is not possibile to form cycles among functional objects. However, even functional languages are not always pure -- OCaml, Scala, Lisp and many other common implementations all provide mutable references locally or even globally.

Project Verona @Verona @parkinson2024reference is another project utilizing RC-managed objects in its language runtime. Project Verona studies scheduling tasks safely and efficiently with concurrency. As such, the language design contains mutable and immutable regions @parkinson2024reference. Workers construct desired objects by scheduling jobs in locally mutable regions and then submit them into globally shared environment. As shared environment can be accessed by threads in parallel, mutations are no longer permitted.

To achieve such transition, when a RC-managed object escapes from a locally mutable area, a "freeze" operation will be applied and make the associate object immutable. As cycles may form inside the mutable region, the freezing procedure is designed to collect objects into multiple SCCs (Strongly Connected Components). While the reference count is recorded in the designated representative object, all other objects in the SCC tracks their path to the representative in a union-find manner. This information can be maintained by pointer tagging hence implies negligible overhead#footnote[To track the allocated objects in a region, one can maitain a singly-linked list with another extra word inside the object header.].

```ml
(*sample code taken from verona-rt*)
let freeze (r) =
  pending = empty_stack
    let rec freeze_inner(x) =
      match rep(x).status with
      | UNMARKED =>
        x.status = PENDING(0);
        pending.push(x);
        for each f in x
          freeze_inner(x.f)
        if (pending.peek() == x)
          pending.pop()
          rep(x).status = RC(1);

      | PENDING(N) =>
        while (union(x, pending.peek()))
          pending.pop()

      | RC(N) =>
          x.status = RC(N+1)
  freeze_inner (r)
```

To some extend, inplace mutation problem has similar characteristics of race-free mutations in concurrent programming. Such mutations are permissible if and only if the mutator holds the exclusive access to the underlying memory locations. Hence, the idea of having local mutable regions can be ported to general settings outside concurrent programming, as long as we have a clear isolation between mutable and immutable sections.

We hope to practice this idea in functional programming. A mutable region, can be abstracted as a monad or a function with certain algebraic effect. When taking out the value from a mutable region, the freezing operation can be performed. 

There are many details to be worked out. For example, objects allocated inside a region are not recycled until the region exits. Therefore, we should allow programmers to use nested regions to achieve fine-grained control of memory reclamation.

```ocaml
let a = region {
  let mut x = Foo x y
  let b = region {
    // ...
    m.y = x
  }
}
```
Should we allow outer-scope mutable objects to permeate into nested regions? The answer should be no. If such object is scanned by the inner freezing operation, it will be marked as frozen. However, as the outer scope still holds the mutable reference, its data can be altered. Hence, we may not allow mutable regions to be directly nested. Rather, they should be segmentated and the unfrozen objects in different regions should be isolated. This indicates that some efforts are needed to design a sound type system to allow local mutability with the global immutable RC-runtime.

We find the idea particularly interesting because it has minimal runtime implications. One can introduce freezable and nonfreezing RC into the language to enable local mutability without dealing with complicated GC or different memory models. As we have discussed before, having such a minimal runtime will provide an easy mechanism to interpolate between the mutable and immutable worlds.
== Loop Carried Exclusivity
On the fastpaths with memory reuse, the RC-based approach can eliminate almost all overhead due to memory management. However, in some scenarios, the checking and branching remain to be  problematic.

The following Lean4 code adds $1.0$ to all elements inside a `FloatArray` that stores elements in a fix-sized contiguous memory area. 

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

Notice that we have carefully crafted the code such that there is no boundary checking. Ideally, with trivial tail-call optimization, the above code should be transformed into a loop feasible for SIMD vectorization. Indeed, `leanc` will transform the code above into the following `C` code where updates are happening in a loop.

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

However, there are several issues stop `add1` from being vectorized. The first problem is that, along the loop, there are two `lean_dec` operation. This is because loop index variables can be potentially boxed in Lean4. Such operations can be eliminated with a better design of value-based types.

The other problem is more fundamental and difficult to resolve. The array updating functions such as `lean_float_array_fset` and `lean_float_array_fget` are implemented with inplace mutability support. We have learned such operations incur a check on exclusivity and a potential slow path that clones the underlying data. Vectorizer cannot conduct optimizations due to the existence of these cold routines.

We hope to examine whether we can hoist the loop invariant exclusivity check out of the tight loop body as demonstrated in @loop-hosting. This should be doable by lifting out the exclusivity check together with the first iteration and then continue with the remaining iterations with checking and cloning eliminated. Such operation introduces loop structures other than tail recursion into functional IR, which can affect other optimizations.
One needs to carefully arrange such passes.

#figure(
  image("figures/loop.png", width: 55%),
  caption: [Exclusivity check hoisting]
) <loop-hosting>

== Borrowing and Stack Promotion

Although RC becomes efficient with reuse analysis, more overhead can be reduced by introducing borrowing. Such techiniques are used in Lean4 @lean-4 @ullrich2020countingimmutablebeansreference already. If we know the subroutines are only to do trivial operations, such as reading an integer out of the managed object, we do not need to hand over the ownership and introduce costly RC checkings. In Lean4, however, the reference type is not exposed to normal functional programming interfaces. Rather, they are only available when calling FFIs. We hope to expose the reference type directly as builtin language features.

This introduces another opportunity that is not yet examined in existing implementations. If a locally created object is only passed as references, we do not even need to allocate the object on heap. We can promote it to the stack to avoid unnecessary allocation and deallocation operations. This is similar to the stack promotion pass in bufferization @Bufferization.

== Unified Framework in MLIR

The last point is more of practical values. Different forms of reuse analysis are used in both Lean4 and Koka without a unified playground similar to the MMTk project for the Garbage Collection Community @1317436. We hope to build up a framework with MLIR, as it provides handy dataflow analysis framework. The structured high-level operations can also be helpful when solving problems such as loop invariant hoisting. Being multi-leveled, once our IR framework is implemented, it immediately become available for others to utilize. Similarly, we may also benefit from the existing `linalg` and `tensor` dialect if we want to expose linear algebra features to our functonal languages.

An interesting point to explore is whether the reuse analysis can be reformulated to a dataflow problem. Each drop operation adds in assignable tokens to the context while each allocation may consume such tokens. This assembles the generating and killing sets in iterative dataflow analysis (IDFA). To fuse the RC increment of fields with the destruction of their container, one can combine alias analysis and dominance analysis. If the source container from which the field is projected is an precise alias of the RC pointer being released and if the increment dominates the destruction in dataflow, then one can cancel the increment with the decrement inside the destruction. 

```asm
# Demo of acquire-release fusion in MLIR
func.func @fusion(%0: !box) -> !rc {
  %ref = reuse_ir.rc.borrow %0 : !box -> !ref
  %proj = reuse_ir.proj %ref [1] : !ref -> !refrc
  %valrc = reuse_ir.load %proj : !refrc -> !rc
  %proj2 = reuse_ir.proj %ref [2] : !ref -> !refrc
  %valrc2 = reuse_ir.load %proj2 : !refrc -> !rc
  reuse_ir.rc.acquire (%valrc2 : !rc)
  reuse_ir.rc.acquire (%valrc : !rc)
  %tk = reuse_ir.rc.release (%0 : !box) : !reuse_ir.nullable<!tk1>
  %tk2 = reuse_ir.rc.release (%valrc2 : !rc) : !reuse_ir.nullable<!tk2>
  reuse_ir.token.free (%tk : !reuse_ir.nullable<!tk1>)
  reuse_ir.token.free (%tk2 : !reuse_ir.nullable<!tk2>)
  return %valrc : !rc
}
# After fusion
func.func @fusion(%0: !box) -> !rc {
  %ref = reuse_ir.rc.borrow %0 : !box -> !ref
  %proj = reuse_ir.proj %ref [1] : !ref -> !refrc
  %valrc = reuse_ir.load %proj : !refrc -> !rc
  %proj2 = reuse_ir.proj %ref [2] : !ref -> !refrc
  %valrc2 = reuse_ir.load %proj2 : !refrc -> !rc
  # fused attribute indicate that fields at certain indices no longer require destruction
  %tk = reuse_ir.rc.release (%0 : !box) fused(1, 2) : !reuse_ir.nullable<!tk1>
  %tk2 = reuse_ir.rc.release (%valrc2 : !rc) : !reuse_ir.nullable<!tk2>
  reuse_ir.token.free (%tk : !reuse_ir.nullable<!tk1>)
  reuse_ir.token.free (%tk2 : !reuse_ir.nullable<!tk2>)
  return %valrc : !rc
}
```

In functional programming, the control flow graphs are of DAG forms. Therefore, there is no need to solve the problem with IDFA. Internalizing RC operations, however, should not be restricted to functional programming, but one may need to think about implications of complex dataflows. The bufferization pipeline discussed above may provide hints on implementations but the memory reuse tokens are "nullable" so when joining them across different flows, it may be harder to keep track of the best assignment that maximize the possibility of successful reuse.

== Locality Implications of Memory Reuse

At a high-level perspective, memory reuse via RC can potentially reduce the amount of memory blocks that a program needs to access as memory resource is directly handed over to newly allocated objects when feasible. Since the amount of memory corelates with the working set size, we should be able to see its effect on locality.

However, there is no existing quantitive study on how reuse analysis affects the locality of programs. There are several difficulties when applying locality analysis to the scenario of memory reuse:
- Memory reuse is nondeterministic. The resource handover only happens if the object reference is exclusive. One may need to introduce a probability model to reason about the locality implications of reused memory blocks.
- Static locality analysis generally favors regular access patterns, such as array and matrices, while reuse analysis may handle large or small objects with different levels of connectivity among them.
- The exact relation between reuse likelihood and locality can be complicated. Having heavily shared objects may reduce the working set size and the chance of reuse in the same time.

As RC-managed runtime is widely used in many applications including some programs that are sensitive to performance, characterizing how reuse analysis affect the cache behavior can be an interesting point for further exploration. Whether memory blocks can be mostly reused or not will differ the working set size of programs. Hence, reuse analysis establish a connection between the complexity of the references among objects and the locality metrics of the programs, which provides a new perspective for future research.

== Security Implications of Memory Reuse
To reduce overhead of reference counting, reuse analysis further differentiate the fastpaths and slowpaths. As such optimization is applied implicity by the compiler, programs may exhibit different behaviors in uncontrolled ways. 

For example, by monitoring allocation frequency, one may be able to tell whether a program is updating a sparse data structure or a dense one. Such differences may lead to further information leakage. Unlike GC or other deferred approaches, RC-based memory reclamation happens timely, which gives more opportunities for observers to infer the information out of the running programs.

Currently, reuse analysis is still being adopted into various languages. If there is any security-critical program to be implemented with reuse analysis, one may need to study how to mask certain allocation patterns by probabilistically reject a reuse opportunity. There may be a tradeoff between the performance and the security.

= Conclusions

For programming languages, immutability is like a black chocolate, tasty but sometimes bitter. The referential transparency enforced by immutability simplifies analyses and enables various optimizations. However, it also forbids most updates to happen inplace and makes many easy and efficient mutable design patterns become complicated and costly in the immutable world. We have covered such scenarios in @functional-programming.

In this survey, we have delved into the studies attempting to solve the performance penalty of immutability without breaking it in visible ways. Traditional solutions utilizes static analysis and RC-encoded runtime information to capture the exclusivity of objects and thus enabling inplace updates without breaking the referential transparency assumptions.

Recent progress on RC-based reuse analysis figures out that reference counting not only carries out the exclusivity in runtime but also provides precise spots where static analysis can kick in
to reduce overhead and capture memory reuse or inplace update opportunities. The simplicity of RC runtime also enables efficient wrappers that manipulate imperative data structures in functional ways.

There is still plenty room to improve with inplace update optimizations. In the last part of the survey, we have listed some potential solutions to allow local mutability in RC runtime without pollute its simplicity, reduce RC checking overhead in tight loops to enable vectorization, and further reduce heap allocations with stack promotion. Locality and security implications demand further investigations.

High-performance functional language implementation is becoming increasingly important for various communities. In the meanwhile, functional features such as immutable objects are adopted into more and more languages. We believe further improvements on the reuse analysis hold significant values for both academic research and industrial applications, enhancing the efficiency and effectiveness of immutable object management and delivering better development experience.
