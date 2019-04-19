#==event that triggers the gene info page
# input$res1 and input$resPageId are activated by javascript
# function resClick() found in "clickdetect.js"
observeEvent(input$res1, {
  num <- as.numeric(gsub("^res(\\d+)", "\\1", input$resPageId))
  gene <- storedRes[num]
  variantTable <- fread(tolower(paste0("varTab/", gene$id, ".txt")))
  #aggregate rows are currently taken from: http://annovar.openbioinformatics.org/en/latest/user-guide/gene/
  aggregateVariantTable <- data.table(`Functional Consequence` = c("All SNVs",
                                                    "frameshift insertion",
                                                    "frameshift deletion",
                                                    "frameshift block substitution",
                                                    "stopgain",
                                                    "stoploss",
                                                    "nonframeshift",
                                                    "nonframeshift deletion",
                                                    "nonframeshift block substitution",
                                                    "nonsynonymous SNV",
                                                    "synonymous SNV",
                                                    "unknown"),
                                      Count = c(nrow(variantTable),
                                                length(which(variantTable$ExonicFunc.refGene == "frameshift_insertion")),
                                                length(which(variantTable$ExonicFunc.refGene == "frameshift_deletion")),
                                                length(which(variantTable$ExonicFunc.refGene == "frameshift_block_substitution")),
                                                length(which(variantTable$ExonicFunc.refGene == "stopgain")),
                                                length(which(variantTable$ExonicFunc.refGene == "stoploss")),
                                                length(which(variantTable$ExonicFunc.refGene == "nonframeshift")),
                                                length(which(variantTable$ExonicFunc.refGene == "nonframeshift_deletion")),
                                                length(which(variantTable$ExonicFunc.refGene == "nonframeshift_block_substitution")),
                                                length(which(variantTable$ExonicFunc.refGene == "nonsynonymous SNV")),
                                                length(which(variantTable$ExonicFunc.refGene == "synonymous SNV")),
                                                length(which(variantTable$ExonicFunc.refGene == "unknown"))
                                                ))
  #aggregateVariantTable$Count <-
  #colnames(aggregateVariantTable) <- c("Phenotype", "Frequency")
  #aggregateVariantTable <- fread(paste0("aggregate/", gene$id, ".txt"))
  output$mainPage <- renderUI(tagList(
    fluidRow(
      column(width = 6,
             h1(gene$id),
             h2(gene$name),
             div("Region:", paste0("Chromosome ", gene$chr, ": ", gene$`37bp1`, "-", gene$`37bp2`), style = "margin-bottom:20px;")
      ),
      column(width = 6,
             div(renderTable(aggregateVariantTable), id = "aggregateVariantTable"))#style = "position:absolute;right:12px"))
    ),
    fluidRow(div(renderDT(variantTable, rownames= FALSE), style = "margin: 12px 50px 50px 12px;"))
  ))
  show(id = "mainPageLink")
  hide(id = "miniSearchBar")
  hide(id = "minisubmit")
  pageState <<- 3
})

#return to search results when clicking return to results
observeEvent(input$returnResults, {
  output$mainPage <- resultPage
  hide(id = "mainPageLink")
  show(id = "miniSearchBar")
  show(id = "minisubmit")
  pageState <<- 2
  #hide(id = "wrapperlogo")
}
)