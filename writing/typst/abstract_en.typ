#let abstract_en() = {
  set page(
    margin: (left: 30mm, right: 30mm, top: 40mm, bottom: 40mm),
    numbering: none,
    number-align: center,
  )

  let body-font = "New Computer Modern"
  let sans-font = "New Computer Modern Sans"

  set text(
    font: body-font, 
    size: 12pt, 
    lang: "en"
  )

  set par(
    leading: 1em,
    justify: true
  )

  
  // --- Abstract (DE) ---
  v(1fr)
  align(center, text(font: body-font, 1em, weight: "semibold", "Abstract"))
  
  text[
    The advancement of formal methods has led to an increased focus on functional programming languages. However, these languages face challenges in efficient memory management, particularly due to the dynamic nature of normalization algorithms that frequently create and discard data structures, necessitating a sophisticated memory management policy that accommodates reuse patterns.

    This work examines prior research on the design and implementation of memory reuse runtime for functional programming languages. We go into the foundamental principles that encode the functional programming computations and explain why memory reuse is essential for performance. We conduct case studies on emerging languages like Lean4 and Koka. The work addresses how ownership semantics within these languages interact with memory reuse, alongside the static and runtime analysis required for effective reuse.

    We propose a comprehensive pass-based framework aimed at facilitating reuse analysis and discuss the challenges associated with integrating such a memory reuse framework into high-level languages, including Rust.
  ]
  
  v(1fr)
}