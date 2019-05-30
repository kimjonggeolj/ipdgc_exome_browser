simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste0(toupper(substring(s, 1,1)), substring(s, 2),
         collapse=" ")
}


observeEvent(input$varClick, {
  var <- variantTable.global[variantTable.global$`Position (Ref/Alt)` == input$varPageId]
  colnames(var) <- c("Position (Ref/Alt)",
                     "Region",
                     "Functional consequence",
                     "Amino acid change",
                     "rsID",
                     "Conditions (ClinVar)",
                     "Clinical significance (ClinVar)",
                     "Exome name",
                     "Frequency (overall)",
                     "Frequency (PD)",
                     "Frequency (PD Parents)",
                     "Frequency (AD)",
                     "Frequency (AD Parents)")
  output$freq.Table <- renderTable(var[,c(9:13)],
                                   digits = -2)
  search.Term <- gsub("(\\d+):(\\d+)-\\d+ \\((\\w+)/(\\w*)\\)", "\\1-\\2-\\3-\\4", var[1,1])
  var$rsID <- ifelse(var$rsID == ".", "", var$rsID)
  output$mainPage <- renderUI(tagList(
    fluidRow(
      column(width = 12,
             h1(tags$b("Variant:"), var$`Position (Ref/Alt)`),
             h2(var$rsID,
                tags$i(paste0("(", toupper(input$resPageId), ")")),
                tags$sup(
                  a(href = paste0(
                    "https://www.ai-omni.com/search=",
                    gsub("(\\d+:\\d+)-\\d+ .*", "chr\\1", var$`Position (Ref/Alt)`),
                    "/page=1"),
                    target = "_blank",
                    "omni"),
                  a(href = paste0("https://gnomad.broadinstitute.org/variant/", search.Term),
                    target = "_blank",
                    "gnomad"),
                  a(href = paste0("https://bravo.sph.umich.edu/freeze3a/hg19/variant/", search.Term),
                    target = "_blank",
                    "bravo"),
                  a(href = paste0("https://www.ncbi.nlm.nih.gov/snp/", var$rsID),
                    target = "_blank",
                    ifelse(var$rsID == "", "", "dbSNP"))
                )),
             div(tags$b("Region:"), simpleCap(var$Region[1]), style = "margin-bottom:20px;"),
             div(tags$b("Amino Acid Change:"),
                 gsub(".*(p\\..*)", "\\1", var$`Amino acid change`),
                 style = "margin-bottom:2px;"),
             div(tags$b("Functional Consequence:"),
                 gsub("_", " ", var$`Functional consequence`),
                 style = "margin-bottom:20px;"),
             ifelse(var$`Conditions (ClinVar)` == ".", "", 
                    tagList(div(tags$b("ClinVar (Conditions):"), var$`Conditions (ClinVar)`))),
             ifelse(var$`Clinical significance (ClinVar)` == ".", "",
                    tagList(div(tags$b("ClinVar (Clinical Significance): "), var$`Clinical significance (ClinVar)`,
                                                                          style = "margin-bottom:50px;"))),
                            
             h3("Frequency Table")
      )
    ),
    fluidRow(column(width = 12,
                    fluidRow(div(tableOutput("freq.Table"))))
    )
  )
  )
  hide("mainPageLink")
  show("genePageLink")
  pageState <<- 4
})

observeEvent(input$returnGene, {
  #num <- as.numeric(gsub("^res(\\d+)", "\\1", input$resPageId))
  gene <- geneList[geneList$id == toupper(input$resPageId)]#storedRes[num]
  variantTable.global <<- fread(tolower(paste0("varTab/", gene$id, ".txt")))
  variantTable <- variantTable.global[, c(1:9)]
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
  hide("genePageLink")
  pageState <<- 3
})

output$mytable = renderTable({
  mtcars[1:4,]
})
