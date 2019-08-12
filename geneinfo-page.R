output$geneBox <- renderUI({
  boxPlus(
    title = "Gene",
    uiOutput("panel2"),
    id = "genebox",
    width = ifelse(
      input$layout,
      6,
      12),
    closable = F,
    status = "warning"
  )
})

#==event that triggers the gene info page
# input$geneClick and input$resPageId are activated by javascript
# function resClick() found in "clickdetect.js"
observeEvent(input$geneClick, {
  #num <- as.numeric(gsub("^res(\\d+)", "\\1", input$resPageId))
  # Check if URL or search table click
  if (runFromURL) {
    searchString <- query[['gene']]
    runFromURL <<- F
  } else {
    searchString <- input$resPageId
  }
  gene <- geneList[grepl(paste0('="', searchString, '"'), geneList$id)]#storedRes[num]
  chrom <- gene$chr[1]
  if (is.na(chrom)) {
    sendSweetAlert(
      session = session,
      title = "Not Found",
      text = "Given gene not found. Please search and select a different gene.",
      type = "warning"
    )
  } else {
    #print(paste0("chromosome", chrom))
    load(paste0("varTab/", "chr", chrom, ".RData"))
    initDat <- eval(as.name(paste0("varDat.chr", chrom)))
    # print(head(initDat))
    print("initDat loaded")
    initDat <- initDat[grepl(toupper(searchString), initDat$`Gene.refGene`)][, c("HG19_ID",
                                                                                 "Start",
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
    variantTable.global <<- initDat[, -"Start"]
    needleData <- initDat[, c("HG19_ID", "Start", "Func.refGene", "ExonicFunc.refGene", "avsnp150")]
    colnames(needleData) <- c("HG19_ID", "Position", "Region", "Functional Consequence", "rsID")
    needleData$`Functional Consequence` <- sapply(needleData$`Functional Consequence`, function (x) {
      if (grepl("^frameshift", x)) {
        "frameshift mutation"
      } else if (grepl("^nonframeshift", x)) {
        "nonframeshift mutation"
      } else if (grepl("^\\.", x)) {
        "NA/unknown"
      } else if (grepl("^unknown", x)) {
        "NA/unknown"
      } else {
        x
      }
      # 
      # switch(x,
      #        `nonsynonymous SNV` = 1,
      #        `.` = 0,
      #        `synonymous SNV` = 0,
      #        `stopgain` = 4,
      #        `nonframeshift deletion` = 2,
      #        `frameshift deletion` = 3,
      #        `frameshift insertion` = 3,
      #        `nonframeshift insertion` = 2,
      #        `unknown` = 0,
      #        `stoploss` = 4,
      #        `nonframeshift block substitution` = 2,
      #        `frameshift block substitution` = 3,
      #        `nonframeshift` = 1
      # )
    })
    
    needleData$y <- sapply(needleData$`Functional Consequence`, function (x) {
      switch(x,
             `nonsynonymous SNV` = 1,
             `NA/unknown` = 0,
             `synonymous SNV` = 0,
             `stopgain` = 4,
             `nonframeshift mutation` = 2,
             `frameshift mutation` = 3,
             `stoploss` = 4
      )
    })

    # needleData$color <- lapply(needleData$y, function (x) {
    #   switch(x,
    #          `0` = "black",
    #          `1` = "green",
    #          `2` = "yellow",
    #          `3` = "magenta",
    #          `4` = "red"
    #   )
    # })
    
    # variantTable.global <<- initDat[grepl(toupper(searchString), initDat$`Gene.refGene`)][, c("HG19_ID",
    #                                                                                              #"HG38_ID",
    #                                                                                              "Func.refGene",
    #                                                                                              "ExonicFunc.refGene",
    #                                                                                              "AAChange.refGene",
    #                                                                                              "Gene.refGene",
    #                                                                                              "avsnp150",
    #                                                                                              "CLNDBN",
    #                                                                                              "CLINSIG",
    #                                                                                              # IPDGC Genomes (hg38) Columns:
    #                                                                                              # "genomes_cases",
    #                                                                                              # "genomes_controls",
    #                                                                                              # "MAF_genomes_case",
    #                                                                                              # "genomes_cases_N",
    #                                                                                              # "MAF_genomes_control",
    #                                                                                              # "genomes_controls_N",
    #                                                                                              # IPDGC Exomes + ReSeq (hg19) Columns:
    #                                                                                              "exome_cases",
    #                                                                                              "exome_controls",
    #                                                                                              "MAF_exome_case",
    #                                                                                              "exome_cases_N",
    #                                                                                              "MAF_exome_control",
    #                                                                                              "exome_controls_N",
    #                                                                                              # "reseq_cases",
    #                                                                                              # "reseq_controls",
    #                                                                                              # "MAF_reseq_case",
    #                                                                                              # "reseq_cases_N",
    #                                                                                              # "MAF_reseq_control",
    #                                                                                              # "reseq_controls_N",
    #                                                                                              # gnomad (hg19) + UKBB (hg38) Columns:
    #                                                                                              "AF",
    #                                                                                              "AF_popmax",
    #                                                                                              "controls_AF_popmax",
    #                                                                                              "AF_male",
    #                                                                                              "AF_female",
    #                                                                                              "AF_afr",
    #                                                                                              "AF_sas",
    #                                                                                              "AF_amr",
    #                                                                                              "AF_eas",
    #                                                                                              "AF_nfe",
    #                                                                                              "AF_fin",
    #                                                                                              "AF_asj",
    #                                                                                              "AF_oth"
    # )]
    print("variantTable G loaded")
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
    # print("variantTable local loaded")
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
                                                                     "nonframeshift insertion",
                                                                     "nonframeshift deletion",
                                                                     "nonframeshift block substitution",
                                                                     "nonsynonymous SNV",
                                                                     "synonymous SNV",
                                                                     "NA/unknown"),
                                        Count = c(nrow(variantTable),
                                                  length(which(variantTable$`Functional consequence` == "frameshift insertion")),
                                                  length(which(variantTable$`Functional consequence` == "frameshift deletion")),
                                                  length(which(variantTable$`Functional consequence` == "frameshift block substitution")),
                                                  length(which(variantTable$`Functional consequence` == "stopgain")),
                                                  length(which(variantTable$`Functional consequence` == "stoploss")),
                                                  length(which(variantTable$`Functional consequence` == "nonframeshift")),
                                                  length(which(variantTable$`Functional consequence` == "nonframeshift insertion")),
                                                  length(which(variantTable$`Functional consequence` == "nonframeshift deletion")),
                                                  length(which(variantTable$`Functional consequence` == "nonframeshift block substitution")),
                                                  length(which(variantTable$`Functional consequence` == "nonsynonymous SNV")),
                                                  length(which(variantTable$`Functional consequence` == "synonymous SNV")),
                                                  length(which(variantTable$`Functional consequence` == "unknown")) + length(which(variantTable$`Functional consequence` == "."))
                                        ))
    # Colorscale for needlePlot + waffle plot
    colorList <- list()
    #colorList[[1]] <- c("synonymous SNV", "nonsynonymous SNV", "nonframeshift mutation", "frameshift mutation", "stopgain", "stoploss", "NA/unknown")
    colorList[[1]] <- c("synonymous SNV" = "#beaed4", "nonsynonymous SNV" = "#386cb0", "nonframeshift mutation" = "#7fc97f", "frameshift mutation" = "#fdc086", "stopgain" = "#ffff99", "stoploss" = "#f0027f",  "NA/unknown" = "#e8e6e4")
    #colorList[[3]] <- c("synonymous SNV", "nonsynonymous SNV", "nonframeshift", "nonframeshift insertion", "nonframeshift deletion", "nonframeshift block substitution", "frameshift insertion", "frameshift deletion", "frameshift block substitution", "stopgain", "stoploss", "NA/unknown")
    colorList[[2]] <- c("synonymous SNV" = "#beaed4", "nonsynonymous SNV" = "#386cb0", "nonframeshift" = "#7fc97f", "nonframeshift insertion" = "#7fc9c9", "nonframeshift deletion" = "#a4c97f", "nonframeshift block substitution" = "#5e915d",  "frameshift insertion" = "#fdc086", "frameshift deletion" = "#fddd86", "frameshift block substitution" = "#fda286", "stopgain" = "#ffff99", "stoploss" = "#f0027f", "NA/unknown" = "#e8e6e4")
    
    nelements <- length(unique(needleData$`Functional Consequence`))
    # needle plot, ggplot version
    output$needlePlot <- renderPlotly({
      p <- ggplot(needleData, aes(x = Position, y = y, fill = `Functional Consequence`)) +
        geom_hline(
          yintercept = 0,
          color = tablecolor()
        ) +
        geom_linerange(
          aes(
            x = Position,
            ymin = 0,
            ymax = y
          ),
          # removed for compatibility with clickable plot
          #position = position_jitter(height = 0L, seed = 1L),
          color = tablecolor()
        ) +
        geom_point(
          aes(
            size = 1
          ),
          #position = position_jitter(height = 0L, seed = 1L),
          color = tablecolor()
          ) +
        scale_fill_manual(
          values = colorList[[1]]#,
          #breaks = colorList[[1]]
        ) +
        theme(
          axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
          axis.text = element_text(color = tablecolor()),
            panel.background=element_rect(fill = tablebgcolor(),
                                          colour = tablebgcolor()
            ),
          axis.title.x = element_text(color = tablecolor()),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_rect(fill = tablebgcolor()),
            legend.key = element_rect(fill = tablebgcolor(),
                                      color = tablebgcolor()
            ),
            #legend.position = c(1.2, 0.2),
            legend.background = element_rect(fill = tablebgcolor()),
            legend.title = element_blank(),# element_text(color = tablecolor()),
            legend.text = element_text(color = tablecolor()))
      event_register(p, 'plotly_click')
      ggplotly(p, tooltip = c("Position", "Functional Consequence"), height = 300, width = NULL
        )
      
      
      # ====== plotly version =====
      # plot_ly(
      #   needleData,
      #   # type = "scatter",
      #   x = ~Start
      # ) %>%
      #   add_trace(
      #     y = ~y,
      #     # color = ~`ExonicFunc.refGene`,
      #     # colors = RColorBrewer::brewer.pal(nelements, "Dark2"),
      #     size = 25,
      #     marker = list(
      #       opacity = 1,
      #       #color = 'red',
      #       line = list(
      #         color = 'black',
      #         width = 1
      #       )
      #     ),
      #     # symbol = ~ExonicFunc.refGene,
      #     # symbols = c('circle','x','o'),
      #     text = ~paste('rsID:', avsnp150, "<br>Position: ", Start, '<br>Functional Consequence:', ExonicFunc.refGene),
      #     mode = 'markers'
      #   ) %>%
      #   add_bars(
      #     y = ~y,
      #     width = 1#,
      #     # color = ~`Func.refGene`,
      #     #colors = "YlOrRd"#,
      #     #mode = 'lines',
      #     #marker = list(
      #       #color = 'black'
      #     #)#,
      #     #showlegend = F
      #   ) %>%
      #   layout(
      #     yaxis = list(
      #       title = "",
      #       zeroline = FALSE,
      #       showline = FALSE,
      #       showticklabels = FALSE#,
      #       #showgrid = FALSE
      #     ),
      #     xaxis = list(
      #       title = "Position",
      #       autotick = FALSE,
      #       ticks = "outside",
      #       #tick0 = 0,
      #       dtick = 10000,
      #       ticklen = 5,
      #       tickwidth = 2,
      #       tickcolor = toRGB("blue"),
      #       zeroline = FALSE,
      #       showgrid = T
      #       # showticklabels = FALSE,
      #       # showgrid = FALSE
      #     )
      #   )
    }
    )
    output$aggregateDonut <- renderPlot({
      # Waffles
      # How many rows do you want the y axis to have?
      donutDat <- aggregateVariantTable[`Functional consequence` != "All SNVs"]
      ndeep <- 10
      # I need to convert my data into a data.frame with uniquely-specified x
      # and y coordinates for each case
      # Note - it's actually important to specify y first for a
      # horizontally-accumulating waffle
      # One y for each row; then divide the total number of cases by the number of
      # rows and round up to get the appropriate number of x increments
      dat <- expand.grid(y = 1:ndeep,
                         x = seq_len(ceiling(sum(donutDat$Count) / ndeep)))
      
      # Expand the counts into a full vector of region labels - i.e., de-aggregate
      regionvec <- rep(donutDat$`Functional consequence`, donutDat$Count)
      
      # Depending on the value of ndeep, there might be more spots on the x-y grid
      # than there are cases - so fill those with NA
      dat$region <- c(regionvec, rep(NA, nrow(dat) - length(regionvec)))
      ggplot(dat, aes(x = x, y = y, fill = region)) + 
        geom_tile(
          color = tablebgcolor(),
          size = 2
          ) + # The color of the lines between tiles
        scale_fill_manual("Changes",
                          values = colorList[[2]]#,
                          #breaks = colorList[[3]]
                          ) +
        labs(title = "Functional Consequence") +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.background=element_rect(fill = tablebgcolor(),
                                            colour = tablebgcolor()
                                            ),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_rect(fill = tablebgcolor(),
                                           color = tablebgcolor()),
              legend.position = 'left',
              legend.key = element_rect(fill = tablebgcolor(),
                                        color = tablebgcolor()
                                        ),
              legend.background = element_rect(fill = tablebgcolor()),
              legend.title = element_text(color = tablecolor()),
              legend.text = element_text(color = tablecolor()),
              plot.title = element_text(face = "bold", hjust = 0.5, color = tablecolor()),
              plot.margin=grid::unit(c(0,0,0,0), "mm")
        )
    })
    #aggregateVariantTable$Count <-
    #colnames(aggregateVariantTable) <- c("Phenotype", "Frequency")
    #aggregateVariantTable <- fread(paste0("aggregate/", gene$id, ".txt"))
    output$panel2 <- renderUI(tagList(
      fluidRow(
        column(width = 6,
               h1(searchString),#gene$id),
               h2(ifelse(is.na(gene$name), "", gene$name)),
               div("Region:", paste0("Chromosome ", gene$chr, ":", gene$`37bp1`, "-", gene$`37bp2`), style = "margin-bottom:20px;"),
               ifelse(grepl("^LOC", searchString) | searchString == "TBC1D7-LOC100130357", "", tagList(div(a("NCBI Genetics Home Reference", href = paste0("https://ghr.nlm.nih.gov/gene/", searchString), target = "_blank")))),
               plotlyOutput(
                 "needlePlot",
                 height = "300px"
               )
        ),
        column(width = 6,
               div(renderTable(aggregateVariantTable), id = "aggregateVariantTable"),
               div(plotOutput(
                 #width = "30%",
                 "aggregateDonut",
                 height = "500px"
               ),
               id = "aggregateVariantTable",
               style = "display:inline;height:100%;width:60%;")
        )#style = "position:absolute;right:12px"))
      ),
      fluidRow(div(renderDT({datatable(variantTable[, c(1:8,11,17)], options = list(paging = F, scrollX = T, scrollY = "500px"), rownames= FALSE, escape = FALSE, selection = 'none') %>% formatStyle(columns=colnames(variantTable[, c(1:8,11,17)])
                                                                                                                                                                                                      , style="bootstrap", backgroundColor = tablebgcolor(), color = tablecolor()#, style = tableCol
      )}), style = "margin: 12px 50px 50px 12px;"))
    ))
  }
})