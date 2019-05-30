observeEvent(input$about, {
  showModal(modalDialog(
    "IPDGC Genome Browser is a database of sequencing variants related to neurodegenerative diseases that can be used by researchers all over the world.",
    hr(),
    h4("Current database statistics"),
    renderTable(data.table(`Total Variant Events` = 11111, `Total Subjects` = 22222)),
    p(pageState),
    p("pixelratio",session$clientData$pixelratio),
    p("wrapperlogowidth", session$clientData$output_wrapperlogo_width),
    p("url_search", session$clientData$url_search),
    p("url_pathname", session$clientData$url_pathname)
  ))
})


#add padding to "about" when clicking "search"
observeEvent(input$submit,
             {addClass(id = "top-row", class = "paddedrow")}
)
#remove padding to "about" when clicking the logo
observeEvent(input$wrapperlogo,
             {removeClass(id = "top-row", class = "paddedrow")})