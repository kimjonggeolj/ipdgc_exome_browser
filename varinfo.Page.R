simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste0(toupper(substring(s, 1,1)), substring(s, 2),
         collapse = " ")
}


observeEvent(input$varClick, {
  var <- variantTable.global[variantTable.global$`HG19_ID` == input$varPageId]
  colnames(var) <- c("Exome name (hg19)",
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
  # output$genome.freq.Table <- renderTable(
  #   var[, c(9:14)],
  #   digits = -2
  # )
  
  output$exome.freq.Table <- renderTable(
    dcast(melt(var[,c(8:13)], id.vars = "Exome-Distribution (cases)"), variable ~ `Exome-Distribution (cases)`),
    digits = -2
  )
  
  # output$reseq.freq.Table <- renderTable(
  #   var[,c(21:26)],
  #   digits = -2
  # )
  
  output$others.freq.Table <- renderTable(
    dcast(melt(var[, c(14:26)], id.vars = "gnomAD Genome Allele Frequency (AF)"), variable ~ `gnomAD Genome Allele Frequency (AF)`),
    digits = -2
  )
  
  var$rsID <- ifelse(var$rsID == ".", "", var$rsID)
  output$panel3 <- renderUI(tagList(
    fluidRow(
      column(width = 12,
             h1(
               tags$b("Variant:"),
               var$`Exome name (hg19)`,
               "(hg19)")#,
             # h1(HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
             #    var$`Exome name (hg38)`,
             #    "(hg38)"
             #    )
      ),
      column(width = 12,
             h2(var$rsID,
                tags$i(paste0("(", toupper(input$resPageId), ")")),
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
                  a(href = paste0("https://bravo.sph.umich.edu/freeze5/hg38/variant/",
                                  gsub("(\\d+):(\\d+):(.*):(.*)", "\\1-\\2-\\3-\\4", var[1,2])
                                       ),
                    target = "_blank",
                    "bravo"),
                  a(href = paste0("https://www.ncbi.nlm.nih.gov/snp/", var$rsID[1]),
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
        width = 6,
        h4("IPDGC"),
        # h5("Genome"),
        # div(
        #   tableOutput("genome.freq.Table")
        # ),
        h5("Exome"),
        div(
          tableOutput("exome.freq.Table")
        )#,
        # h5("Reseq"),
        # div(
        #   tableOutput("reseq.freq.Table ")
        # )
      ),
      column(
        width = 6,
        h4("Other Resources"),
            div(
              tableOutput("others.freq.Table")
            )
      )
    )#,
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
})