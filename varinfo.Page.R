simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste0(toupper(substring(s, 1,1)), substring(s, 2),
         collapse = " ")
}


observeEvent(input$varClick, {
  var <- variantTable.global[variantTable.global$`HG19_ID` == input$varPageId]
  colnames(var) <- c("Exome name (hg19)",
                     "Exome name (hg38)",
                     "Region",
                     "Functional consequence",
                     "Amino acid change",
                     "rsID",
                     "Conditions (ClinVar)",
                     "Clinical significance (ClinVar)",
                     # IPDGC Genomes (hg38) Columns:
                     "Genomes-Distribution (cases)",
                     "Genomes-Distribution (control)",
                     "Genomes-MAF (cases)",
                     "Genomes-number of participants (cases)",
                     "Genomes-MAF (control)",
                     "Genomes-number of participants (control)",
                     # IPDGC Exomes + ReSeq (hg19) Columns:
                     "Exome-Distribution (cases)",
                     "Exome-Distribution (control)",
                     "Exome-MAF (cases)",
                     "Exome-number of participants (cases)",
                     "Exome-MAF (control)",
                     "Exome-number of participants (control)",
                     "Reseq-Distribution (cases)",
                     "Reseq-Distribution (controls)",
                     "Reseq-MAF (case)",
                     "Reseq-number of participants (cases)",
                     "Reseq-MAF (control)",
                     "Reseq-number of participants (controls)",
                     # gnomad (hg19) + UKBB (hg38) Columns:
                     "gnomAD Genome Allele Frequency (AF)",
                     "gnomAD Popmax Filtering AF",
                     "gnomAD Popmax Filtering AF-Controls",
                     "gnomAD AF-male",
                     "gnomAD AF-female",
                     "gnomAD AF-African",
                     "gnomAD AF-South Asian",
                     "gnomAD AF-Latino",
                     "gnomAD AF-East Asian",
                     "gnomAD AF-European (non-Finnish)",
                     "gnomAD AF-Finnish",
                     "gnomAD AF-Ashkenazi Jewish",
                     "gnomAD AF-Other")
  output$genome.freq.Table <- renderTable(
    var[, c(9:14)],
    digits = -2
  )
  
  output$exome.freq.Table <- renderTable(
    var[,c(15:20)],
    digits = -2
  )
  
  output$reseq.freq.Table <- renderTable(
    var[,c(21:26)],
    digits = -2
  )
  
  output$others.freq.Table <- renderTable(
    var[, c(27:39)],
    digits = -2
  )
  
  search.Term <- gsub("(\\d+):(\\d+)-\\d+ \\((\\w+)/(\\w*)\\)", "\\1-\\2-\\3-\\4", var[1,1])
  var$rsID <- ifelse(var$rsID == ".", "", var$rsID)
  output$panel3 <- renderUI(tagList(
    fluidRow(
      column(width = 12,
             h1(
               tags$b("Variant:"),
               var$`Exome name (hg19)`,
               "(hg19)"),
             h1(HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
                var$`Exome name (hg38)`,
                "(hg38)")
      ),
      column(width = 12,
             h2(var$rsID,
                tags$i(paste0("(", toupper(input$resPageId), ")")),
                tags$sup(
                  a(href = paste0(
                    "https://www.ai-omni.com/search=",
                    gsub("(\\d+:\\d+)-\\d+ .*", "chr\\1", var$`Exome name (hg19)`),
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
    fluidRow(
      column(
        width = 12,
        h4("IPDGC"),
        h5("Genome"),
        div(
          tableOutput("genome.freq.Table")
        ),
        h5("Exome"),
        div(
          tableOutput("exome.freq.Table")
        ),
        h5("Reseq"),
        div(
          tableOutput("reseq.freq.Table ")
        )
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 12,
        h4("Other Resources"),
        div(
          tableOutput("others.freq.Table")
        )
      )
    )
  )
  )
  show("genePageLink")
})

observeEvent(input$returnGene, {
  #num <- as.numeric(gsub("^res(\\d+)", "\\1", input$resPageId))
  gene <- geneList[geneList$id == toupper(input$resPageId)]#storedRes[num]
  variantTable.global <<- fread(tolower(paste0("varTab/", gene$id, ".txt")))[, c("HG19_ID",
                                                                                 "HG38_ID",
                                                                                 "Func.refGene",
                                                                                 "ExonicFunc.refGene",
                                                                                 "AAChange.refGene",
                                                                                 "avsnp150",
                                                                                 "CLNDBN",
                                                                                 "CLINSIG",
                                                                                 # IPDGC Genomes (hg38) Columns:
                                                                                 "genomes_cases",
                                                                                 "genomes_controls",
                                                                                 "MAF_genomes_case",
                                                                                 "genomes_cases_N",
                                                                                 "MAF_genomes_control",
                                                                                 "genomes_controls_N",
                                                                                 # IPDGC Exomes + ReSeq (hg19) Columns:
                                                                                 "exome_cases",
                                                                                 "exome_controls",
                                                                                 "MAF_exome_case",
                                                                                 "exome_cases_N",
                                                                                 "MAF_exome_control",
                                                                                 "exome_controls_N",
                                                                                 "reseq_cases",
                                                                                 "reseq_controls",
                                                                                 "MAF_reseq_case",
                                                                                 "reseq_cases_N",
                                                                                 "MAF_reseq_control",
                                                                                 "reseq_controls_N",
                                                                                 # gnomad (hg19) + UKBB (hg38) Columns:
                                                                                 "AF",
                                                                                 "AF_popmax",
                                                                                 "controls_AF_popmax",
                                                                                 "AF_male",
                                                                                 "AF_female",
                                                                                 "AF_afr",
                                                                                 "AF_sas",
                                                                                 "AF_amr",
                                                                                 "AF_eas",
                                                                                 "AF_nfe",
                                                                                 "AF_fin",
                                                                                 "AF_asj",
                                                                                 "AF_oth"
  )]
  variantTable <- variantTable.global
  colnames(variantTable) <- c("Exome name (hg19)",
                              "Exome name (hg38)",
                              "Region",
                              "Functional consequence",
                              "Amino acid change",
                              "rsID",
                              "Conditions (ClinVar)",
                              "Clinical significance (ClinVar)",
                              # IPDGC Genomes (hg38) Columns:
                              "Genomes-Distribution (cases)",
                              "Genomes-Distribution (control)",
                              "Genomes-MAF (cases)",
                              "Genomes-number of participants (cases)",
                              "Genomes-MAF (control)",
                              "Genomes-number of participants (control)",
                              # IPDGC Exomes + ReSeq (hg19) Columns:
                              "Exome-Distribution (cases)",
                              "Exome-Distribution (control)",
                              "Exome-MAF (cases)",
                              "Exome-number of participants (cases)",
                              "Exome-MAF (control)",
                              "Exome-number of participants (control)",
                              "Reseq-Distribution (cases)",
                              "Reseq-Distribution (controls)",
                              "Reseq-MAF (case)",
                              "Reseq-number of participants (cases)",
                              "Reseq-MAF (control)",
                              "Reseq-number of participants (controls)",
                              # gnomad (hg19) + UKBB (hg38) Columns:
                              "gnomAD Genome Allele Frequency (AF)",
                              "gnomAD Popmax Filtering AF",
                              "gnomAD Popmax Filtering AF-Controls",
                              "gnomAD AF-male",
                              "gnomAD AF-female",
                              "gnomAD AF-African",
                              "gnomAD AF-South Asian",
                              "gnomAD AF-Latino",
                              "gnomAD AF-East Asian",
                              "gnomAD AF-European (non-Finnish)",
                              "gnomAD AF-Finnish",
                              "gnomAD AF-Ashkenazi Jewish",
                              "gnomAD AF-Other")
  for (i in 1:nrow(variantTable)) {
    variantTable$`Exome name (hg19)`[i] <- paste0('<a id="',  variantTable$`Exome name (hg19)`[i], '" href="#" onclick="varClick(this.id)">', variantTable$`Exome name (hg19)`[i], '</a>')
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
  output$panel2 <- renderUI(tagList(
    fluidRow(
      column(width = 6,
             h1(gene$id),
             h2(gene$name),
             div("Region:", paste0("Chromosome ", gene$chr, ": ", gene$`37bp1`, "-", gene$`37bp2`), style = "margin-bottom:20px;")
      ),
      column(width = 6,
             div(renderTable(aggregateVariantTable), id = "aggregateVariantTable"))#style = "position:absolute;right:12px"))
    ),
    fluidRow(div(renderDT({datatable(variantTable[, c(1:8,11,17)], rownames= FALSE, escape = FALSE, selection = 'none') %>% formatStyle(columns=colnames(variantTable[, c(1:8,11,17)])
                                                                                                                                        , style="bootstrap", backgroundColor = tablebgcolor(), color = tablecolor()#, style = tableCol
    )}), style = "margin: 12px 50px 50px 12px;"))
  ))
  hide("genePageLink")
})

output$mytable = renderTable({
  mtcars[1:4,]
})
