observeEvent(input$about, {
  showModal(modalDialog(
    "IPDGC Genome Browser is a database of sequencing variants related to neurodegenerative diseases that can be used by researchers all over the world.",
    hr(),
    h4("Current database statistics"),
    renderTable(data.table(`Total Variant Events` = 11111, `Total Subjects` = 22222)),
    p(pageState)
  ))
})


#add padding to "about" when clicking "search"
observeEvent(input$submit,
             {addClass(id = "top-row", class = "paddedrow")}
)
#remove padding to "about" when clicking the logo
observeEvent(input$wrapperlogo,
             {removeClass(id = "top-row", class = "paddedrow")})