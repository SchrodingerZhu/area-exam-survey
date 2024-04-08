#let titlepage(
  title: "",
  degree: "",
  program: "",
  advisor: "",
  author: "",
  submissionDate: none,
) = {
  set document(title: title, author: author)
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

  set par(leading: 1em)

  
  // --- Title Page ---
  v(1cm)
  align(center, image("../figures/ur-logo.svg", width: 33%))

  v(5mm)
  align(center, text(font: sans-font, 2em, weight: 700, "University of Rochester"))

  v(5mm)
  align(center, text(font: sans-font, 1.5em, weight: 100, "Hajim School of Engineering and Applied Sciences \n -- Computer Science --"))
  
  v(15mm)

  align(center, text(font: sans-font, 1.3em, weight: 100, "Area Exam"))
  v(8mm)
  

  align(center, text(font: sans-font, 2em, weight: 700, title))

  pad(
    top: 3em,
    right: 15%,
    left: 15%,
    grid(
      columns: 2,
      gutter: 1em,
      strong("Author: "), author,
      strong("Academic Advisor: "), advisor,
      strong("Submission Date: "), submissionDate,
    )
  )

  pagebreak()
}