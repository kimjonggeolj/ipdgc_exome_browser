#==event that triggers the gene info page
# input$geneClick and input$resPageId are activated by javascript
# function resClick() found in "clickdetect.js"
observeEvent(input$geneClick, {
  #num <- as.numeric(gsub("^res(\\d+)", "\\1", input$resPageId))
  gene <- geneList[geneList$id == toupper(input$resPageId)]#storedRes[num]
  variantTable <- fread(tolower(paste0("varTab/", gene$id, ".txt")))[, c(1:9)]
  colnames(variantTable) <- c("Position (Ref/Alt)",
                              "Region",
                              "Functional consequence",
                              "Amino acid change",
                              "rsID",
                              "Conditions (ClinVar)",
                              "Clinical significance (ClinVar)",
                              "Exome name",
                              "Frequency (overall)")
  for (i in 1:nrow(variantTable)) {
    variantTable$`Position (Ref/Alt)`[i] <- paste0('<a id="',  variantTable$`Position (Ref/Alt)`[i], '" href="#" onclick="varClick(this.id)">', variantTable$`Position (Ref/Alt)`[i], '</a>')
    #restoreInput(id = paste0("res", i), default = NULL)
  }
  #aggregate rows are currently taken from: http://annovar.openbioinformatics.org/en/latest/user-guide/gene/
  aggregateVariantTable <- data.table(`Functional consequence` = c("All SNVs",
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
                                                length(which(variantTable$`Functional consequence` == "frameshift_insertion")),
                                                length(which(variantTable$`Functional consequence` == "frameshift_deletion")),
                                                length(which(variantTable$`Functional consequence` == "frameshift_block_substitution")),
                                                length(which(variantTable$`Functional consequence` == "stopgain")),
                                                length(which(variantTable$`Functional consequence` == "stoploss")),
                                                length(which(variantTable$`Functional consequence` == "nonframeshift")),
                                                length(which(variantTable$`Functional consequence` == "nonframeshift_deletion")),
                                                length(which(variantTable$`Functional consequence` == "nonframeshift_block_substitution")),
                                                length(which(variantTable$`Functional consequence` == "nonsynonymous_SNV")),
                                                length(which(variantTable$`Functional consequence` == "synonymous_SNV")),
                                                length(which(variantTable$`Functional consequence` == "unknown"))
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
    fluidRow(div(renderDT({datatable(variantTable, rownames= FALSE, escape = FALSE) %>% formatStyle(columns=colnames(variantTable)
                                                                                                    , style="bootstrap", backgroundColor = tablebgcolor(), color = tablecolor()#, style = tableCol
    )}), style = "margin: 12px 50px 50px 12px;"))
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