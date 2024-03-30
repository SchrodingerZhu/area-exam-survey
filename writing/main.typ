#import "template.typ": *
#import "common/titlepage.typ": *
#import "typst/openning.typ": *
#import "typst/disclaimer.typ": *
#import "typst/acknowledgement.typ": *
#import "typst/abstract_en.typ": *
#import "common/metadata.typ": *
#import "@preview/fletcher:0.4.2" as fletcher: node, edge

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

= The Memory Reuse Problem
== The Essence of Memory Reuse

In the previous section, we observed examples featuring frequent invocations of introduction and elimination processes. Within the language runtime, this translates into allocations and deallocations of memory objects. Consequently, memory reuse emerges as a crucial aspect of performance improvement. As we will explore in the following sections, this concept is reflected from various angles in runtime implementations.

== Memory Reuse in Allocators

In the userspace of modern Unix-like operating systems, memory is typically obtained from the kernel in page granularity through system call wrappers such as mmap @mmap. However, employing mmap/munmap for each allocation and deallocation can be prohibitively expensive. In the same time, mmap can only reserve page-length virtual address spaces, potentially leading to significant fragmentation.

To counteract these issues, userspace memory is generally managed by memory allocators, acting as a sort of cache for pages between the operating system and userspace allocations. Upon allocation, rather than directly invoking mmap, allocators opt to utilize available space in their cached pages. Conversely, upon deallocation, instead of immediately returning the page to the OS, the allocator postpones the page's release by initially placing it into the cache pool. This practice represents a lower-level form of memory reuse, aiming to repurpose available page space from previous deallocations in the hope that subsequent allocations will reuse this memory space.

Memory allocators, like those incorporated by libc, are often accessed by multiple threads, necessitating the global page pool to be safeguarded by mutexes or other synchronization mechanisms, thereby imposing additional costs on malloc/free operations. To circumvent this overhead, modern allocators such as mimalloc, referenced in @leijen2019mimalloc, and snmalloc, noted in @snmalloc, have introduced a concept known as freelist sharding. This approach involves segregating the page pool by different size classes, with available memory blocks being placed into local freelists initially and only transferred back to the global pool once certain conditions are met. This strategy adds an additional layer of memory reuse by considering the local context, where consecutive allocations and deallocations are capured by frequent addition and removal of memory objects from local lists, thereby achieving higher throughput.

Especially for small allocations on the fast path, where an allocation finds a match in local freelists, the operation is exceptionally efficient; for instance, in snmalloc, there are only about 45 general instructions with just two branches, significantly fewer than what is required for the cold initialization routine.

#let flowgraph = { 
import fletcher.shapes: diamond
  fletcher.diagram(
    node-stroke: 1pt,
    edge-stroke: 1pt,
    node((0,0), [Malloc], corner-radius: 2pt, extrude: (0, 3)),
    edge("-|>"),
    node((0,1), align(center)[
      Initialized?
    ], shape: diamond),
    edge("d", "-|>", [Yes], label-pos: 0.1),
    node((0,2), align(center)[
      Local freelist available?
    ], shape: diamond),
    edge("d", "-|>", [Yes], label-pos: 0.1),
    node((0,3), [Return local block], corner-radius: 2pt, extrude: (0, 3)),
  )
  h(1cm)
  fletcher.diagram(
    node-stroke: 1pt,
    edge-stroke: 1pt,
    node((0,0), [Free], corner-radius: 2pt, extrude: (0, 3)),
    edge("-|>"),
    node((0,1), align(center)[
      Is Local Block?
    ], shape: diamond),
    edge("d", "-|>", [Yes], label-pos: 0.1),
    node((0,2), align(center)[
      Local freelist not full?
    ], shape: diamond),
    edge("d", "-|>", [Yes], label-pos: 0.1),
    node((0,3), [Append to local list], corner-radius: 2pt, extrude: (0, 3)),
  )
}

#figure(flowgraph, caption: "Fast paths for snmalloc")

== Reuse Analysis

=== Precise Reference Counting for Reuse Analysis

Functional languages like Erlang, Haskell, and OCaml utilize sophisticated garbage collection algorithms, despite differences in their implementations, as noted in @haskell, @erlang-1, @erlang-2, @ocaml-pm, and @ocaml. These garbage collectors are all based on the generational copying GC principle. The generational approach, to some extent, mirrors reuse patterns; for instance, as discussed in @haskell, the promotion from the younger to older generations follows a tenuring model. The "weak generational hypothesis" posits that objects in the young generation are more likely to be reclaimed frequently, reflecting the transient nature of data in functional programming through the cycle of introduction and elimination.

However, due to the batched nature of garbage collection, memory reuse is generally delayed, leading to potential inefficiencies when compared to imperative data structures. This is primarily because:

1. Imperative data structures are usually modified in place, as opposed to being completely recreated, thus avoiding the cycle of elimination and introduction inherent in functional structures.
2. Deallocations in imperative programming are more explicit and precise, leading to potentially more efficient memory use.

To approach the functional equivalent of these characteristics, beyond efficient reclamation, one also needs to ensure the uniqueness (or exclusivity) of managed objects. Here, RC-based (Reference Counting) strategies excel, as recent works in @perceus, @frame-limited, and @fp2 have started to establish a comprehensive set of RC-based reuse analysis and optimizations.

Compared to complex GC runtimes, RC is simpler and more straightforward. For example, the inductively defined integer list reversing function can be translated into Rust using `Rc` in a standard manner as illustrated:

#text(size: 12pt)[
```rs
pub fn reverse(xs: Rc<List>, acc: Rc<List>) -> Rc<List> {
  match *xs {
    List::Nil => acc,
    List::Cons(ref x, ref xs) => 
      everse(xs.clone(), Rc::new(List::Cons(x.clone(), acc)))
  }
}
```
]

However, with Rc, several additional operations are necessary. New memory cells must be allocated (`Rc::new`) for new objects, reference counts of existing objects must be incremented before they can be shared (`Rc::clone`), and reference counts should decrease when objects are no longer in use, potentially triggering deallocations (`Rc::drop`).

In Rust and C++, `Rc` or `shared_pointer` is nothing but a normal structure defined in the standard library. Their constructions and deconstructions are treated in the same way of other objects. That is, the `drop` operation, if no explicit specified, will be inserted by the compiler when exiting the lexical scope of the `Rc` object. 

Koka and Lean internalize `Rc` as a part of IR; thus allowing manipulations of the `Rc` object. In the above example, such compilers will identify the frontier of uses of the `Rc` object and insert the `drop` operations for them as soon as they are no longer being used. 

#text(size: 12pt)[
```rs
pub fn reverse(xs: Rc<List>, acc: Rc<List>) -> Rc<List> {
  match *xs {
    List::Nil => acc,
    List::Cons(ref y, ref ys) => {
      let y = y.clone();
      let ys = ys.clone();
      drop(xs);
      reverse(ys, Rc::new(List::Cons(y, acc)))
    }
  }
}
```
]
This code motion has several benefits. One a memory cell is no longer referenced, it will be released before subsequent calls to constructors. Consider a data structure not widely shared, such early release together with the local freelist technique from allocators will enable possible reuse of memory cells.

Unfortunately, such code motions also incur substantial costs.
1. In Rust (and C++), `Rc` are usually constructed with the hope that it may be shared at various sites. Hence, its deallocation is annotated as a cold path, potentially confusing the branch prediction;
2. Even though the memory reuse is possible, the calls to allocations and deallocations may not be easily canceled. C++ does permit cancellation of `new` and `delete` pairs, but the compiler support is rather restrictive;
3. Meanwhile, such code always projects out subfields via clone operations no matter the memory reuse is feasible or not (or, whether the `Rc` is holding the exclusive reference to the object). In the case of exclusive access, there is no need to do `clone` followed by `drop` for subfields.

To mitigate these issues, one can inline the destructors associated with the `List`. To make sure the memory cell is reused immediately, we also explicitly express the passing of memory. Although the following Rust code is conceptual and not directly admissible by standard Rust compilers, it serves to illustrate the underlying idea:

#text(size: 12pt)[
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
]

As an optimization, the `clone()` operations could be moved inside the `Rc::is_unique` branch to pair with the `drop()` operations, eliminating unnecessary cloning when the Rc is unique. This modification leads to a cleaner and more efficient path for direct memory reuse:

#text(size: 12pt)[
```rs
pub fn reverse(xs: Rc<List>, acc: Rc<List>) -> Rc<List> {
  match xs {
    List::Nil => acc,
    List::Cons(ref y, ref ys) => {
      let mem = if Rc::is_unique(xs) {
        Some(Rc::take_memory(xs))
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

=== Frame-limited Reuse Analysis

To assess the potential for reusability, the initial strategy adopted in Koka, as outlined in @perceus, involves attempting to drop the memory right after pattern destruction. This action signifies the memory resource as available for reuse. This concept is closely aligned with the notion that pattern destructions serve as typical elimination points for inductive data structures. However, as highlighted in @frame-limited, this approach can encounter significant challenges that may hinder the effective reuse of memory.

Consider the following conceptual code (notice that `clone()`, `drop()` are not yet inserted):

#text(size: 12pt)[
```rs 
pub fn foo(bar: Rc<List>, baz: bool) -> Rc<List> {
  match bar {
    List::Cons(hd, tl) => {
      match baz {
        true => bar,
        false => List::Cons(1, tl),
      }
    }
    _ => omitted!()
  }
}
```
]

Inserting the `drop()` operation immediately after pattern destruction is not always feasible, particularly when the variable in question is utilized within a nested branch. A viable workaround is to defer the `drop()` to subsequent layers of the program. This kind of code transformation can be systematically executed, a process that will be elaborated on in the context of "drop-guided" release analysis, as discussed in @frame-limited. Before delving into that discussion, let's examine another motivating example to further illustrate the concept.

#text(size: 12pt)[
```rs 
pub fn foo(bar: Rc<List>, baz: bool) -> Rc<List> {
  match bar {
    List::Cons(_, _) => {
      let qux = quux(bar)
      Rc::new(Cons(qux, Nil))
    }
    _ => omitted!()
  }
}
```
]

If the reuse analysis is too "optimistic", one may end up getting the following code:
#text(size: 12pt)[
```rs 
let qux = quux(bar.clone())
let token = drop_with_memory(bar);
Rc::reuse_or_alloc(token, Cons(qux, Nil))
``` 
] 
While this code captures the reuse opportunity, it introduces another problem. That is, even though the memory associated with `bar` may have already become available for reuse before calling `quux`, it is not released during the entire execution of `quux`. In the worst-case scenario, this could result in a continuous increase in heap usage, especially if quux is a long-running operation or if similar patterns are prevalent throughout the codebase, leading to inefficient memory utilization.

@frame-limited believes that problems mentioned above fundamentally stem from a disregard for liveness in reuse analysis. The reuse analysis pass already acquire essential information of the liveness of objects. Hence, the reuse decisions  decisions should be more closely aligned with this liveness data. As shown in @analysis-flow, the proposed algorithm leverages liveness information to pinpoint the frontier where a managed object is used for the last time. Drops are inserted to such sites. In this way, if the ownership of an object is passed to another function, neither `drop()` nor `clone()` will be added, avoiding the problem in `quux(bar.clone())`. To achieve memory reuse, a `drop()` in this case, will always generate a reuse token carried by the context. Going along the control flow, if there is an allocation that is feasible to reuse memory resource carried within the context, the reuse token will be assigned to the allocation. Conversely, if there is no possible memory reuse, an additional `free()` operation will be inserted to clean up any surplus tokens.

#let analysis-flow = { 
import fletcher.shapes: diamond
  fletcher.diagram(
    node-stroke: 1pt,
    edge-stroke: 1pt,
    node((0,0), [Input IR], corner-radius: 2pt, extrude: (0, 3)),
    edge("->"),
    node((1,0), align(center)[
      Last Use Frontier
    ]),
    edge("->", label-pos: 0.1),
    node((2,0), align(center)[
      Insert Drop
    ]),
    edge("->", label-pos: 0.1),
    node((3, 0), [Pair Possible Reuse], ),
  )
}
#figure(
analysis-flow,
caption: "Frame-limited Reuse Analysis Flow"
)<analysis-flow>

== e.g. User Feedback
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: This section would summarize the concept User Feedback using definitions, historical overviews and pointing out the most important aspects of User Feedback.
]

== e.g. Representational State Transfer
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: This section would summarize the architectural style Representational State Transfer (REST) using definitions, historical overviews and pointing out the most important aspects of the architecture.
]

== e.g. Scrum
#rect(
  width: 100%,
  radius: 10%,
  stroke: 0.5pt,
  fill: yellow,
)[
  Note: This section would summarize the agile method Scrum using definitions, historical overviews and pointing out the most important aspects of Scrum.
]

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