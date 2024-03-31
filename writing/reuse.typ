#let reuse-section = {
import "@preview/fletcher:0.4.2" as fletcher: node, edge
text[
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
]
}