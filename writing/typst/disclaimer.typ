#let disclaimer(
  title: "",
  author: "",
  submissionDate: none,
) = {
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

  v(75%)
  text("I confirm that this area paper is my own work and I have documented all sources and material used. The wording may have been grammatically checked and polished by tools including ChatGPT and Grammarly, but the content is original and has not been fabricated or suggested by generative AI.")

  v(15mm)
  grid(
      columns: 2,
      gutter: 1fr,
      "Rochester, NY, USA, " + submissionDate, author
  )
}
