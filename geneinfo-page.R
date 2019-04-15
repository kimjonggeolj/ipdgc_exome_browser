#==event that triggers the gene info page
# input$res1 and input$resPageId are activated by javascript
# function resClick() found in "clickdetect.js"
observeEvent(input$res1, {
  num <- as.numeric(gsub("^res(\\d+)", "\\1", input$resPageId))
  gene <- storedRes[num]
  output$mainPage <- renderUI(tagList(
    fluidRow(
      column(width = 6,
             h1(gene$id[num]),
             h2(gene$name[1]),
             div("Region:", paste0("Chromosome ", gene$chr, ": ", gene$`37bp1`, "-", gene$`37bp2`), style = "margin-bottom:20px;")
      ),
      column(width = 6,
             div(renderTable(aggregateVariantTable), style = "position:absolute;right:12px"))
    ),
    fluidRow(div(renderDT(variantTable), style = "margin: 12px 50px 50px 12px;"))
  ))
  show(id = "mainPageLink")
})

#return to search results when clicking return to results
observeEvent(input$returnResults, {
  output$mainPage <- resultPage
  hide(id = "mainPageLink")
  #hide(id = "wrapperlogo")
}
)