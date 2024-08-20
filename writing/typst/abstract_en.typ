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
    This work examines prior research on the design and implementation that solves inplace update problems of functional programming languages. The survey covers common design patterns in functional languages that impose performance penalty due to immutability. To find answers to such issue, both traditional static analysis and newly developed RC-oriented optimizations are studied and carefully compared. The survey also overviews some potential future improvements that can be implemented in addition to existing reuse analysis framework.
  ]
  
  v(1fr)
}
