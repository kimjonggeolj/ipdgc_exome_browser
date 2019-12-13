simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste0(toupper(substring(s, 1,1)), substring(s, 2),
         collapse = " ")
}

createVarFunc <- function(searchString = searchString) {
  print('createVarFunc started!')
  print(paste('searchString is', searchString))
  chrom <- gsub("^(\\d{1,2}):\\d+:.*$", "\\1", searchString)
  print(paste('chromosome is', chrom))
  # print(chrom)
  # variantFolder <- ifelse(isDemo, "demodata/varTab/", "data/varTab/")
  load(paste0(variantFolder, "chr", chrom, ".RData"))
  var <- eval(as.name(paste0("varDat.chr", chrom)))
  var[var$`HG19_ID` == searchString][, c("HG19_ID",
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
}

varPageFunc <- function(var = var) {
  print(head(var))
  colnames(var) <- c("Exome name (hg19)",
                     # "Exome name (hg38)",
                     "Region",
                     "Functional consequence",
                     "Amino acid change",
                     "Nearest Gene",
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
                     "Genome Allele Frequency (AF)", #gnomAD====================
                     "Popmax Filtering AF",
                     "Popmax Filtering AF-Controls",
                     "AF-male",
                     "AF-female",
                     "AF-African",
                     "AF-South Asian",
                     "AF-Latino",
                     "AF-East Asian",
                     "AF-European (non-Finnish)",
                     "AF-Finnish",
                     "AF-Ashkenazi Jewish",
                     "AF-Other"                     #gnomAD ends here===========
  )
  # output$genome.freq.Table <- renderTable(
  #   var[, c(9:14)],
  #   digits = -2
  # )
  
  output$exome.freq.Table <- renderTable(
    {
      table.case <- dcast(melt(var[,c(8,9,11,12)], id.vars = "Clinical significance (ClinVar)"), variable ~ `Clinical significance (ClinVar)`)
      colnames(table.case) <- c("Population", "Case")
      table.case$Population <- gsub(' \\(\\w+\\)', '', table.case$Population)
      table.control <- dcast(melt(var[,c(8,10,13,14)], id.vars = "Clinical significance (ClinVar)"), variable ~ `Clinical significance (ClinVar)`)
      colnames(table.control) <- c("Population", "Control")
      table.control$Population <- gsub(' \\(\\w+\\)', '', table.control$Population)
      table <- merge(table.case, table.control, by = 'Population')
      colnames(table)[1] <- ""
      table
    },
    digits = -2
  )
  
  # output$reseq.freq.Table <- renderTable(
  #   var[,c(21:26)],
  #   digits = -2
  # )
  
  output$others.freq.Table <- renderTable(
    {
      table <- dcast(melt(var[, c(14:27)], id.vars = "Exome-number of participants (control)"), variable ~ `Exome-number of participants (control)`)
      colnames(table) <- c("Population", "Frequency")
      table$Population <- gsub('gnomAD ', '', table$Population)
      table$Population <- gsub(' Allele Frequency \\(AF\\)', '', table$Population)
      table$Population <- gsub(' AF', '', table$Population)
      table$Population <- gsub('AF-', '', table$Population)
      #table$Population <- gsub('Other', 'Other Populations', table$Population)
      table
    },
    digits = -2
  )
  
  output$varShareLink <- renderText({paste0(
    session$clientData$url_protocol,
    "//",
    session$clientData$url_hostname,
    ":",
    session$clientData$url_port,
    "?variant=",
    #"https://pdgenetics.shinyapps.io/LRRK2Browser/?gene=",
    var$`Exome name (hg19)`)})
  
  var$rsID <- ifelse(var$rsID == ".", "", var$rsID)
  
  output$panel3 <- renderUI(tagList(
    fluidRow(
      column(width = 12,
             h1(
               tags$b(
                 "Variant"
               ),
               tags$sup(
                 actionLink(
                   "varShareButton",
                   icon = icon("share-alt"),
                   label = ""
                 )
               ),
               hidden(
                 absolutePanel(
                   id = "varSharePanel",
                   verbatimTextOutput("varShareLink")
                 )
               ),
               span(var$`Exome name (hg19)`, id = "variantName"),
               "(hg19)")#,
             # h1(HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
             #    var$`Exome name (hg38)`,
             #    "(hg38)"
             #    )
      ),
      column(width = 12,
             h2(var$rsID,
                tags$i(
                  HTML(
                    paste0("(",
                           a(
                             onClick = "resClick(this.id)",
                             href = "javascript:;",
                             id = ifelse(str_count(var$`Nearest Gene`[1], ";") == 0, var$`Nearest Gene`[1], gsub("^(\\w+);.+$", "\\1", var$`Nearest Gene`[1])),
                             toupper(ifelse(str_count(var$`Nearest Gene`[1], ";") == 0, var$`Nearest Gene`[1], gsub("^(\\w+);.+$", "\\1", var$`Nearest Gene`[1]))#input$resPageId
                             )
                           ),
                           ")")
                  )
                ),
                tags$sup(
                  a(href = paste0(
                    "https://www.ai-omni.com/search=",
                    gsub("(\\d+:\\d+).+", "chr\\1", var[1,1]),
                    "/page=1"),
                    target = "_blank",
                    "omni"),
                  a(href = paste0("https://gnomad.broadinstitute.org/variant/", var[1,1]),
                    target = "_blank",
                    "gnomad"),
                  a(href = paste0("https://bravo.sph.umich.edu/freeze3a/hg19/variant/",
                                  gsub("(\\d+):(\\d+):(.*):(.*)", "\\1-\\2-\\3-\\4", var[1,1])
                  ),
                  target = "_blank",
                  "bravo"),
                  # BRAVO for hg38 freeze5
                  # a(href = paste0("https://bravo.sph.umich.edu/freeze5/hg38/variant/",
                  #                 gsub("(\\d+):(\\d+):(.*):(.*)", "\\1-\\2-\\3-\\4", var[1,2])
                  #                      ),
                  #   target = "_blank",
                  #   "bravo"),
                  a(href = paste0("https://www.ncbi.nlm.nih.gov/snp/", var$rsID[1]),
                    target = "_blank",
                    ifelse(var$rsID == "", "", "dbSNP"))
                ))#,
             # div(tags$b("Region:"), simpleCap(var$Region[1]), style = "margin-bottom:20px;"),
             # div(tags$b("Amino Acid Change:"),
             #     gsub(".*(p\\..*)", "\\1", var$`Amino acid change`),
             #     style = "margin-bottom:2px;"),
             # div(tags$b("Functional Consequence:"),
             #     gsub("_", " ", var$`Functional consequence`),
             #     style = "margin-bottom:20px;"),
             # ifelse(var$`Conditions (ClinVar)` == ".", "", 
             #        tagList(div(tags$b("ClinVar (Conditions):"), var$`Conditions (ClinVar)`))),
             # ifelse(var$`Clinical significance (ClinVar)` == ".", "",
             #        tagList(div(tags$b("ClinVar (Clinical Significance): "), var$`Clinical significance (ClinVar)`,
             #                    style = "margin-bottom:50px;")))#,
             
             #h2("Frequency Table")
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 4,
        descriptionBlock(
          header = "REGION",
          text = simpleCap(var$Region[1]),
          right_border = T,
          margin_bottom = F
        )
      ),
      column(
        width = 4,
        descriptionBlock(
          header = "AMINO ACID CHANGE",
          text = gsub(".*p\\.(.*)", "\\1", var$`Amino acid change`),
          right_border = T,
          margin_bottom = F
        )
      ),
      column(
        width = 4,
        descriptionBlock(
          header = "FUNCTIONAL CONSEQUENCE",
          text = gsub("_", " ", var$`Functional consequence`),
          right_border = FALSE,
          margin_bottom = F
        )
      ),
    ),
    fluidRow(
      column(
        width = 6,
        descriptionBlock(
          header = "ClinVar (Conditions)",
          text = ifelse(
            var$`Conditions (ClinVar)` == ".",
            "N/A",
            gsub(
              ",",
              ", ",
              gsub(
                "\\|",
                " | ",
                var$`Conditions (ClinVar)`
              )
            )
          ),
          right_border = T,
          margin_bottom = F
        )
      ),
      column(
        width = 6,
        descriptionBlock(
          header = "ClinVar (Clinical Significance)",
          text = ifelse(
            var$`Conditions (ClinVar)` == ".",
            "N/A",
            gsub(
              ",",
              ", ",
              gsub(
                "\\|",
                " | ",
                var$`Clinical significance (ClinVar)`
              )
            )
          ),
          right_border = FALSE,
          margin_bottom = F
        )
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 6,
        h3("IPDGC"),
        # h5("Genome"),
        # div(
        #   tableOutput("genome.freq.Table")
        # ),
        h4("Exome"),
        div(
          tableOutput("exome.freq.Table")
        ),
      ),
      column(
        width = 6,
        h3("Other Resources"),
        h4(a("gnomAD", href = "https://gnomad.broadinstitute.org/", target = "_blank")),
        div(
          tableOutput("others.freq.Table")
        )
      )#,
      # column(
      #   width = 6,
      #   boxPad(
      #     color = "blue",
      #     descriptionBlock(
      #       header = "REGION", 
      #       text = simpleCap(var$Region[1]), 
      #       right_border = FALSE,
      #       margin_bottom = TRUE
      #     ),
      #     descriptionBlock(
      #       header = "AMINO ACID CHANGE", 
      #       text = gsub(".*(p\\..*)", "\\1", var$`Amino acid change`), 
      #       right_border = FALSE,
      #       margin_bottom = TRUE
      #     ),
      #     descriptionBlock(
      #       header = "FUNCTIONAL CONSEQUENCE", 
      #       text = gsub("_", " ", var$`Functional consequence`), 
      #       right_border = FALSE,
      #       margin_bottom = FALSE
      #     )
      #   ),
      #   boxPad(
      #     color = "teal",
      #     descriptionBlock(
      #       header = "ClinVar (Conditions)", 
      #       text = ifelse(var$`Conditions (ClinVar)` == ".", "N/A", var$`Conditions (ClinVar)`), 
      #       right_border = FALSE,
      #       margin_bottom = TRUE
      #     ),
      #     descriptionBlock(
      #       header = "ClinVar (Clinical Significance)", 
      #       text = ifelse(var$`Clinical significance (ClinVar)` == ".", "N/A", var$`Clinical significance (ClinVar)`), 
      #       right_border = FALSE,
      #       margin_bottom = TRUE
      #     ),
      #   )
      # )#,
      # hr(),
      # fluidRow(
      #   column(
      #     width = 12,
      #     h4("Other Resources"),
      #     div(
      #       tableOutput("others.freq.Table")
      #     )
      #   )
      # )
    )
  )
  )
  show(id = "draggable-top")
  runjs(
    '
  $(".draggable").position({
    my: "center center+25%",
    at: "center",
    of: window,
    using: function (pos, ext) {
        $(this).animate({ top: pos.top }, 400);
    }
})

    '
  )
  # print('varPageFunc totally ran!')
}

observeEvent(input$varShareButton,
             {
               toggle(id = "varSharePanel")
             })

observeEvent(input$hide.draggable.top, {
  hide(id = "draggable-top")
})

observeEvent(input$varClick, {
  if (runfromPlotly) {
    var <- plotlyVariantTable
    runfromPlotly <<- F
  } else {
    var <- variantTable.global[variantTable.global$`HG19_ID` == input$varPageId]
  }
  varPageFunc(var)
  #shinyjs::show("varBox")
})

observeEvent(input$varResClick, {
  # print("varResClick start")
  # switch to see if running from URL or not
  if (runFromURL) {
    searchString <- query[['variant']]
    runFromURL <<- F
  } else {
    searchString <- input$varResPageId
  }
  var <- createVarFunc(searchString)
  varPageFunc(var)
  # var <- variantTable.global[variantTable.global$`HG19_ID` == input$varPageId]
  #shinyjs::show("varBox")
})