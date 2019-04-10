observeEvent(input$submit, {
  output$mainPage <- renderUI(tagList(
    fluidRow(
      column(width = 6,
        h1("SAMPLE-GENEID"),
        h2("SAMPLE-GENENAME"),
        div("Region:")
        ),
      column(width = 6,
             div(renderTable(aggregateVariantTable), style = "position:absolute;right:12px"))
      ),
    fluidRow(renderDT(variantTable))
  ))
  show(id = "mainPageLink")
})

aggregateVariantTable <- data.table("Variant Type" = c("SAMPLE", "SAMPLE"), "Count" = c(1234, 1234))

variantTable <- data.table("Alleles (rsIDs)" = c("A/C (rsXXXXXXXXXX)"), "Position" = c("YYYYYYYYY"), "Consequence" = c("missense"), "CADD" = "", "N Alleles" = "ZZZZZZ", "HomAlt" = 0, "Frequency" = "1%")

observeEvent(input$main, {
  output$mainPage <- initmainPage
  hide(id = "mainPageLink")
}
             )