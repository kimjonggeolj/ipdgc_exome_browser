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
  # load(paste0(variantFolder, "chr", chrom, ".RData"))
  var <- fread(paste0(variantFolder, "chr", chrom, ".tsv"))# eval(as.name(paste0("varDat.chr", chrom)))
  var[var$`HG19_ID` == searchString]
}

varPageFunc <- function(var = var) {
  #print(head(var))
  colnames(var) <- c("Exome name (hg19)",
                     "Distribution (cases)",
                     "Distribution (control)",
                     "Distribution (proxy)",
                     "A1-frequency (cases)",
                     "A1-frequency (control)",
                     "A1-frequency (proxy)",
                     # "Exome name (hg38)",
                     "Region",
                     "Nearest Gene",
                     "Gene Detail",
                     "Functional consequence",
                     "Amino acid change",
                     "rsID",
                     "Conditions (ClinVar)",
                     "Clinical significance (ClinVar)",
                     "gnomAD Genome Allele Frequency (AF)",
                     "gnomAD Popmax Filtering AF",
                     "gnomAD AF-male",
                     "gnomAD AF-female",
                     "gnomad AF_raw",
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
  
  
  
  
  output$overall.freq.Table <- renderTable(
    {
      table.case <- dcast(melt(var[,c(1,2,5)], id.vars = "Exome name (hg19)"), variable ~ `Exome name (hg19)`)
      colnames(table.case) <- c("Population", "Case")
      table.case$Population <- gsub(' \\(\\w+\\)', '', table.case$Population)
      table.control <- dcast(melt(var[,c(1,3,6)], id.vars = "Exome name (hg19)"), variable ~ `Exome name (hg19)`)
      colnames(table.control) <- c("Population", "Control")
      table.control$Population <- gsub(' \\(\\w+\\)', '', table.control$Population)
      table.proxy <- dcast(melt(var[,c(1,4,7)], id.vars = "Exome name (hg19)"), variable ~ `Exome name (hg19)`)
      colnames(table.proxy) <- c("Population", "Proxy")
      table.proxy$Population <- gsub(' \\(\\w+\\)', '', table.proxy$Population)
      table <- merge(table.case, table.control, by = 'Population')
      table <- merge(table, table.proxy, by = 'Population')
      colnames(table)[1] <- ""
      table
    },
    digits = -2
  )
  
  varMini <- var[,"Exome name (hg19)"]
  
  cohort_dist <- paste0("data/cohort_specific/chr", gsub("^(\\d+):.*$", "\\1", varMini[1,1]), ".tsv.gz") %>% vroom(delim = "\t", col_types = "cccnnccnnccnnccnncccnnn")
  print(varMini)
  print(head(cohort_dist))
  cohort_dist <- as.data.table(filter(cohort_dist, grepl(varMini[1,1], `SNP`) ))
  print(cohort_dist)
  
  generate_freq_table <- function(cohort){
    if (cohort == "genome" ) {
      table.freq <- cohort_dist[1,c(1,4,5)]
      table.count <- cohort_dist[1,c(1:3)]
    } else if (cohort == "exome") {
      table.freq <- cohort_dist[,c(1,8,9)]
      table.count <- cohort_dist[,c(1,6,7)]
    } else if (cohort == "reseq") {
      table.freq <- cohort_dist[,c(1,12,13)]
      table.count <- cohort_dist[,c(1,10,11)]
    } else if (cohort == "imputed") {
      table.freq <- cohort_dist[,c(1,16,17)]
      table.count <- cohort_dist[,c(1,14,15)]
    } else if (cohort == "UKB") {
      table.freq <- cohort_dist[,c(1,21:23)]
      table.count <- cohort_dist[,c(1,18:20)]
    }
    column_name <- if (cohort == "UKB") { c('', 'Case', 'Control', 'Proxy') } else { c('', 'Case', 'Control') }
    colnames(table.freq) <- column_name
    colnames(table.count) <- column_name
    table <- rbind(table.freq, table.count)
    table[1,1] <- 'A1-frequency'
    table[2,1] <- 'Distribution'
    colnames(table)[1] <- ""
    return(table)
  }
  
  genome.table <- generate_freq_table("genome")
  exome.table <- generate_freq_table("exome")
  reseq.table <- generate_freq_table("reseq")
  imputed.table <- generate_freq_table("imputed")
  UKB.table <- generate_freq_table("UKB")
  
  
  output$genome.freq.Table <- renderTable(
    {
      genome.table
    }
  )
  
  output$exome.freq.Table <- renderTable(
    {
      exome.table
    }
  )
  
  output$reseq.freq.Table <- renderTable(
    {
      reseq.table
    }
  )
  
  output$imputed.freq.Table <- renderTable(
    {
      imputed.table
    }
  )
  
  output$UKB.freq.Table <- renderTable(
    {
      UKB.table
    }
  )
  # output$reseq.freq.Table <- renderTable(
  #   var[,c(21:26)],
  #   digits = -2
  # )
  
  output$others.freq.Table <- renderTable(
    {
      table <- dcast(melt(var[, c(15:28)], id.vars = "Clinical significance (ClinVar)"), variable ~ `Clinical significance (ClinVar)`)
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
  
  output$freq.table.DL <- downloadHandler(
    filename = "PD_var_freq_table.csv",
    content = function(file) {
      fwrite(cohort_dist, file)
    }
  )
  
  output$varShareLink <- renderText({paste0(
    session$clientData$url_protocol,
    "//",
    session$clientData$url_hostname,
    session$clientData$url_port,
    session$clientData$url_pathname,
    "?variant=",
    #"https://pdgenetics.shinyapps.io/LRRK2Browser/?gene=",
    var$`Exome name (hg19)`)})
  
  var$rsID <- ifelse(var$rsID == ".", "", var$rsID)
  
  output$panel3 <- renderUI(tagList(
    fluidRow(
      column(width = 12,
             h1(
               tags$b(
                 "Variant: "
               ),
               span(
                 var$`Exome name (hg19)`, id = "variantName"
                 ),
               tags$sup(
                 actionLink(
                   "varShareButton",
                   icon = icon("link"),
                   label = ""
                 )
               ),
               hidden(
                 absolutePanel(
                   id = "varSharePanel",
                   verbatimTextOutput("varShareLink")
                 )
               ),
               #"(hg19)"
               )#,
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
          rightBorder = T,
          marginBottom = F
        )
      ),
      column(
        width = 4,
        descriptionBlock(
          header = "EXON + AMINO ACID CHANGE",
          text = var$`Amino acid change`,#str_wrap(var$`Amino acid change`, width = 20),#gsub(".*p\\.(.*)", "\\1", var$`Amino acid change`),
          rightBorder = T,
          marginBottom = F
        )
      ),
      column(
        width = 4,
        descriptionBlock(
          header = "FUNCTIONAL CONSEQUENCE",
          text = gsub("_", " ", var$`Functional consequence`),
          rightBorder = FALSE,
          marginBottom = F
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
          rightBorder = T,
          marginBottom = F
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
          rightBorder = FALSE,
          marginBottom = F
        )
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 6,
        h3("IPDGC + UKB frequencies and allele counts",
           tags$sup(downloadBttn(
             outputId = "freq.table.DL",
             size = "xs",
             label = "CSV",
             style = "simple"
           ))
        ),
        # h5("Genome"),
        # div(
        #   tableOutput("genome.freq.Table")
        # ),
        h4("Overall"),
        div(
          tableOutput("overall.freq.Table")
        ),
        h4("PD Genome"),
        div(
          tableOutput("genome.freq.Table")
        ),
        h4("PDGSC"),
        div(
          tableOutput("exome.freq.Table")
        ),
        h4("Resequencing project"),
        div(
          tableOutput("reseq.freq.Table")
        ),
        h4("Imputed array"),
        div(
          tableOutput("imputed.freq.Table")
        ),
        h4("UKB"),
        div(
          tableOutput("UKB.freq.Table")
        )
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
      #       rightBorder = FALSE,
      #       marginBottom = TRUE
      #     ),
      #     descriptionBlock(
      #       header = "AMINO ACID CHANGE", 
      #       text = gsub(".*(p\\..*)", "\\1", var$`Amino acid change`), 
      #       rightBorder = FALSE,
      #       marginBottom = TRUE
      #     ),
      #     descriptionBlock(
      #       header = "FUNCTIONAL CONSEQUENCE", 
      #       text = gsub("_", " ", var$`Functional consequence`), 
      #       rightBorder = FALSE,
      #       marginBottom = FALSE
      #     )
      #   ),
      #   boxPad(
      #     color = "teal",
      #     descriptionBlock(
      #       header = "ClinVar (Conditions)", 
      #       text = ifelse(var$`Conditions (ClinVar)` == ".", "N/A", var$`Conditions (ClinVar)`), 
      #       rightBorder = FALSE,
      #       marginBottom = TRUE
      #     ),
      #     descriptionBlock(
      #       header = "ClinVar (Clinical Significance)", 
      #       text = ifelse(var$`Clinical significance (ClinVar)` == ".", "N/A", var$`Clinical significance (ClinVar)`), 
      #       rightBorder = FALSE,
      #       marginBottom = TRUE
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
  show(id = "draggable-top-var")
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
  tutorial <<- "variant"
  if (input$tutorial.mode == T) {
    output$tutorial <- tutorial.variant
  }
  # print('varPageFunc totally ran!')
}

observeEvent(input$varShareButton,
             {
               toggle(id = "varSharePanel")
             })

observeEvent(input$hide.draggable.top.var, {
  hide(id = "draggable-top-var")
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