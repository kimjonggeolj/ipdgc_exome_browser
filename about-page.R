observeEvent(input$about, {
  showModal(modalDialog(
    size = "l",
    "IPDGC Genome Browser is a database of sequencing variants related to neurodegenerative diseases that can be used by researchers all over the world.",
    hr(),
    h4("Current database statistics"),
    renderTable(data.table(`Total Variant Events` = "INSERT NUM HERE", `Total Subjects` = "INSERT NUM HERE")),
    h4("Debug data"),
    p("pixelratio",session$clientData$pixelratio),
    p("wrapperlogowidth", session$clientData$output_wrapperlogo_width),
    p("url_search", session$clientData$url_search),
    p("url_pathname", session$clientData$url_pathname),
    p("session", str(reactiveValuesToList(input))),
    fluidRow(
      column(
        width = 3,
        a(img(src = "ipdgc.png", style = "height:50px;"), href = "https://pdgenetics.org/", targe = "_blank")
        ),
      column(
        width = 3,
        a(img(src = "nia-logo.png", style = "height:50px;"), href = "https://www.nia.nih.gov/", targe = "_blank"))
      )
  ))
})


#add padding to "about" when clicking "search"
# observeEvent(input$submit,
#              {addClass(id = "top-row", class = "paddedrow")}
# )
# #remove padding to "about" when clicking the logo
# observeEvent(input$wrapperlogo,
#              {removeClass(id = "top-row", class = "paddedrow")})