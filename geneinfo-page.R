#==event that triggers the gene info page
# input$geneClick and input$resPageId are activated by javascript
# function resClick() found in "clickdetect.js"
observeEvent(input$geneClick, {
  #num <- as.numeric(gsub("^res(\\d+)", "\\1", input$resPageId))
  gene <- geneList[grepl(paste0('="', input$resPageId, '"'), geneList$id)]#storedRes[num]
  chrom <- gene$chr[1]
  #print(paste0("chromosome", chrom))
  load(paste0("varTab/", "chr", chrom, ".RData"))
  initDat <- eval(as.name(paste0("varDat.chr", chrom)))
  # print(head(initDat))
  print("initDat loaded")
  variantTable.global <<- initDat[grepl(toupper(input$resPageId), initDat$`Gene.refGene`)][, c("HG19_ID",
                                                                                               #"HG38_ID",
                                                                                               "Func.refGene",
                                                                                               "ExonicFunc.refGene",
                                                                                               "AAChange.refGene",
                                                                                               "Gene.refGene",
                                                                                               "avsnp150",
                                                                                               "CLNDBN",
                                                                                               "CLINSIG",
                                                                                               # IPDGC Genomes (hg38) Columns:
                                                                                               # "genomes_cases",
                                                                                               # "genomes_controls",
                                                                                               # "MAF_genomes_case",
                                                                                               # "genomes_cases_N",
                                                                                               # "MAF_genomes_control",
                                                                                               # "genomes_controls_N",
                                                                                               # IPDGC Exomes + ReSeq (hg19) Columns:
                                                                                               "exome_cases",
                                                                                               "exome_controls",
                                                                                               "MAF_exome_case",
                                                                                               "exome_cases_N",
                                                                                               "MAF_exome_control",
                                                                                               "exome_controls_N",
                                                                                               # "reseq_cases",
                                                                                               # "reseq_controls",
                                                                                               # "MAF_reseq_case",
                                                                                               # "reseq_cases_N",
                                                                                               # "MAF_reseq_control",
                                                                                               # "reseq_controls_N",
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
  print("variantTable G loaded")
  #print(head(variantTable.global))
  
  
  
  
  #fread(tolower(paste0("varTab/",
  #                                              tolower(input$resPageId),#gene$id,
  #                                              ".txt")))[, c("HG19_ID",
  #                                                                                "HG38_ID",
  #                                                                                "Func.refGene",
  #                                                                                "ExonicFunc.refGene",
  #                                                                                "AAChange.refGene",
  #                                                                                "avsnp150",
  #                                                                                "CLNDBN",
  #                                                                                "CLINSIG",
  #                                                                                # IPDGC Genomes (hg38) Columns:
  #                                                                                "genomes_cases",
  #                                                                                "genomes_controls",
  #                                                                                "MAF_genomes_case",
  #                                                                                "genomes_cases_N",
  #                                                                                "MAF_genomes_control",
  #                                                                                "genomes_controls_N",
  #                                                                                # IPDGC Exomes + ReSeq (hg19) Columns:
  #                                                                                "exome_cases",
  #                                                                                "exome_controls",
  #                                                                                "MAF_exome_case",
  #                                                                                "exome_cases_N",
  #                                                                                "MAF_exome_control",
  #                                                                                "exome_controls_N",
  #                                                                                "reseq_cases",
  #                                                                                "reseq_controls",
  #                                                                                "MAF_reseq_case",
  #                                                                                "reseq_cases_N",
  #                                                                                "MAF_reseq_control",
  #                                                                                "reseq_controls_N",
  #                                                                                # gnomad (hg19) + UKBB (hg38) Columns:
  #                                                                                "AF",
  #                                                                                "AF_popmax",
  #                                                                                "controls_AF_popmax",
  #                                                                                "AF_male",
  #                                                                                "AF_female",
  #                                                                                "AF_afr",
  #                                                                                "AF_sas",
  #                                                                                "AF_amr",
  #                                                                                "AF_eas",
  #                                                                                "AF_nfe",
  #                                                                                "AF_fin",
  #                                                                                "AF_asj",
  #                                                                                "AF_oth"
  # )]
  variantTable <- variantTable.global[, -c("Gene.refGene")]
  colnames(variantTable) <- c("Exome name (hg19)",
                              # "Exome name (hg38)",
                              "Region",
                              "Functional consequence",
                              "Amino acid change",
                              "rsID",
                              "Conditions (ClinVar)",
                              "Clinical significance (ClinVar)",
                              # IPDGC Genomes (hg38) Columns:
                              # "Genomes-Distribution (cases)",
                              # "Genomes-Distribution (control)",
                              # "Genomes-MAF (cases)",
                              # "Genomes-number of participants (cases)",
                              # "Genomes-MAF (control)",
                              # "Genomes-number of participants (control)",
                              # IPDGC Exomes + ReSeq (hg19) Columns:
                              "Exome-Distribution (cases)",
                              "Exome-Distribution (control)",
                              "Exome-MAF (cases)",
                              "Exome-number of participants (cases)",
                              "Exome-MAF (control)",
                              "Exome-number of participants (control)",
                              # "Reseq-Distribution (cases)",
                              # "Reseq-Distribution (controls)",
                              # "Reseq-MAF (case)",
                              # "Reseq-number of participants (cases)",
                              # "Reseq-MAF (control)",
                              # "Reseq-number of participants (controls)",
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
  print("variantTable local loaded")
  for (i in 1:nrow(variantTable)) {
    variantTable$`Exome name (hg19)`[i] <- paste0('<a id="',  variantTable$`Exome name (hg19)`[i], '" href="javascript:;" onclick="varClick(this.id)">', variantTable$`Exome name (hg19)`[i], '</a>')
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
                                                length(which(variantTable$`Functional consequence` == "frameshift insertion")),
                                                length(which(variantTable$`Functional consequence` == "frameshift deletion")),
                                                length(which(variantTable$`Functional consequence` == "frameshift block substitution")),
                                                length(which(variantTable$`Functional consequence` == "stopgain")),
                                                length(which(variantTable$`Functional consequence` == "stoploss")),
                                                length(which(variantTable$`Functional consequence` == "nonframeshift")),
                                                length(which(variantTable$`Functional consequence` == "nonframeshift deletion")),
                                                length(which(variantTable$`Functional consequence` == "nonframeshift block substitution")),
                                                length(which(variantTable$`Functional consequence` == "nonsynonymous SNV")),
                                                length(which(variantTable$`Functional consequence` == "synonymous SNV")),
                                                length(which(variantTable$`Functional consequence` == "unknown"))
                                      ))
  #aggregateVariantTable$Count <-
  #colnames(aggregateVariantTable) <- c("Phenotype", "Frequency")
  #aggregateVariantTable <- fread(paste0("aggregate/", gene$id, ".txt"))
  output$panel2 <- renderUI(tagList(
    fluidRow(
      column(width = 6,
             h1(input$resPageId),#gene$id),
             h2(gene$name),
             div("Region:", paste0("Chromosome ", gene$chr, ": ", gene$`37bp1`, "-", gene$`37bp2`), style = "margin-bottom:20px;")
      ),
      column(width = 6,
             div(renderTable(aggregateVariantTable), id = "aggregateVariantTable"))#style = "position:absolute;right:12px"))
    ),
    fluidRow(div(renderDT({datatable(variantTable[, c(1:8,11,17)], options = list(scrollX = T), rownames= FALSE, escape = FALSE, selection = 'none') %>% formatStyle(columns=colnames(variantTable[, c(1:8,11,17)])
                                                                                                                                                                     , style="bootstrap", backgroundColor = tablebgcolor(), color = tablecolor()#, style = tableCol
    )}), style = "margin: 12px 50px 50px 12px;"))
  ))
  #show(id = "mainPageLink")
  #hide(id = "miniSearchBar")
  #hide(id = "minisubmit")
  #pageState <<- 3
})

#return to search results when clicking return to results
# observeEvent(input$returnResults, {
#   output$mainPage <- resultPage
#   #hide(id = "mainPageLink")
#   #show(id = "miniSearchBar")
#   #show(id = "minisubmit")
#   #pageState <<- 2
#   #hide(id = "wrapperlogo")
# }
# )