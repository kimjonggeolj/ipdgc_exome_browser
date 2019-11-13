#==event that triggers the gene info page
# Two ways to trigger:
# 1. Javascript function resClick() in search results table (found in "clickdetect.js")
# 2. URL "?gene=[geneName]"

# Both methods trigger input$geneClick


observeEvent(input$geneClick, {
  # Check if URL or search table click
  if (runFromURL) {
    searchString <- query[['gene']]
    runFromURL <<- F
  } else {
    searchString <- input$resPageId
  }
  # Pulls out the gene information from the geneList object
  gene <- geneList[grepl(paste0('="', searchString, '"'), geneList$id)]
  chrom <- gene$chr[1]
  # Error message if gene does not exist
  # Likely happening if URL id given is misspelled or outright wrong
  if (is.na(chrom)) {
    sendSweetAlert(
      session = session,
      title = "Not Found",
      text = "Given gene not found. Please search and select a different gene.",
      type = "warning"
    )
  } else {
    # ======================================================================
    # From here, the process goes like this:
    # 1. Load the proper data table (DT) (DTs are separated by chromosome numbers)
    # 2. Extract all variants related to the gene into a local DT for use
    # 3. Push variant DT to the global environment so that it can be used for the variant box
    # 4. Format the local DT for graphs and tables
    # 5. Generate graphs and tables
    # 6. Generate and update UI
    # 7. If hidden, show the UI
    # ======================================================================
    
    
    # === Debug script ===
    #print(paste0("chromosome", chrom))
    # ====================
    
    # ========== STEP 1 ==========
    
    load(paste0("varTab/", "chr", chrom, ".RData"))
    initDat <- eval(as.name(paste0("varDat.chr", chrom)))
    # === Debug scripts ===
    # print(head(initDat))
    # print("initDat loaded")
    # =====================
    
    # ========== STEP 2 ==========
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
    
    # ========== STEP 3 ==========
    variantTable.global <<- initDat[, -"Start"]
    # debugTable <<- initDat
    
    # ========== STEP 4 ==========
    # FOR NEEDLE PLOT
    needleData <- initDat[, c("HG19_ID", "Func.refGene", "ExonicFunc.refGene", "avsnp150")]
    colnames(needleData) <- c("HG19_ID", "Region", "Functional Consequence", "rsID")
    needleData$Position <- as.numeric(gsub("^\\d+:(\\d+):.+$", "\\1", needleData$HG19_ID))
    needleData <- needleData[, c("HG19_ID", "Position", "Region", "Functional Consequence", "rsID")]
    # Simplifying functional consequence for the needle plot
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
     }
    )
    # Assigning y levels for needle plot
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
    # print("variantTable G loaded")
    
    # FOR VARIANT TABLE
    
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
      if (nchar(variantTable$`Exome name (hg19)`[i]) > 19) {
        variantTable$`Exome name (hg19)`[i] <- paste0('<a id="', variantTable$`Exome name (hg19)`[i], '" href="javascript:;" onclick="varClick(this.id)">', gsub("^(.{20}).*$", "\\1...", variantTable$`Exome name (hg19)`[i]), '</a>')
      } else {
        variantTable$`Exome name (hg19)`[i] <- paste0('<a id="', variantTable$`Exome name (hg19)`[i], '" href="javascript:;" onclick="varClick(this.id)">', variantTable$`Exome name (hg19)`[i], '</a>')
      }
    }
    
    # FOR FUNCTIONAL CONSEQUENCE SUMMARY TABLE
    #     aggregate rows are currently taken from: http://annovar.openbioinformatics.org/en/latest/user-guide/gene/
    aggregateVariantTable <- data.table(Count = c(nrow(variantTable),
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
                                        ),
                                        `Functional consequence` = c("All SNVs",
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
                                                                     "NA/unknown")
                                        )
    # ========== STEP 5 ==========
    # Colorscale for needlePlot + waffle plot
    colorList <- list()
    colorList[[1]] <- c("synonymous SNV" = "#beaed4", "nonsynonymous SNV" = "#386cb0", "nonframeshift mutation" = "#7fc97f", "frameshift mutation" = "#fdc086", "stopgain" = "#ffff99", "stoploss" = "#f0027f",  "NA/unknown" = "#e8e6e4")
    colorList[[2]] <- c("synonymous SNV" = "#beaed4", "nonsynonymous SNV" = "#386cb0", "nonframeshift" = "#7fc97f", "nonframeshift insertion" = "#7fc9c9", "nonframeshift deletion" = "#a4c97f", "nonframeshift block substitution" = "#5e915d",  "frameshift insertion" = "#fdc086", "frameshift deletion" = "#fddd86", "frameshift block substitution" = "#fda286", "stopgain" = "#ffff99", "stoploss" = "#f0027f", "NA/unknown" = "#e8e6e4")
    
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
          # position = position_jitter(height = 0L, seed = 1L),
          color = tablecolor()
        ) +
        geom_point(
          aes(
            size = 1
          ),
          # position = position_jitter(height = 0L, seed = 1L),
          color = tablecolor()
        ) +
        scale_fill_manual(
          values = colorList[[1]]#,
          # breaks = colorList[[1]]
        ) +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(face="bold",
                                     # size=14,
                                     angle=45,
                                     color = tablecolor()
                                     ),
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
      ggplotly(p, tooltip = c("Position", "Functional Consequence"), height = 400, width = NULL
      )
      
      
      # ====== plotly version =====
      # nelements <- length(unique(needleData$`Functional Consequence`))
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
    
    # Waffle plot
    
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
        ) +
        # The color of the lines between tiles
        scale_fill_manual("Changes",
                          values = colorList[[2]],
                          guide = guide_legend(
                            ncol = 1
                          )
                          # breaks = colorList[[3]]0
        ) +
        labs(title = "SNV Type Distribution") +
        theme(
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_rect(
            fill = tablebgcolor(),
            colour = tablebgcolor()
          ),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_rect(
            fill = tablebgcolor(),
            color = tablebgcolor()
          ),
          plot.title = element_text(face = "bold", hjust = 0.5, color = tablecolor()),
          legend.position = 'none'#'bottom',
          # legend.key = element_rect(
          #   fill = tablebgcolor(),
          #   color = tablebgcolor()
          # ),
          # legend.background = element_rect(fill = tablebgcolor()),
          # legend.title = element_blank(),
          # legend.text = element_text(color = tablecolor()),
          
          #,
          #plot.margin=grid::unit(c(0,0,0,0), "mm")
        )
    })
    
    # ========== STEP 6 ==========
    
    output$geneInfo <- renderUI(tagList(
      h1(searchString),#gene$id),
      h2(ifelse(is.na(gene$name), "", gene$name)),
      # div("Region:", paste0("Chromosome ", gene$chr, ":", gene$`37bp1`, "-", gene$`37bp2`), style = "margin-bottom:20px;"),
    ))
    
    output$geneNumbers <- renderUI(tagList(
      fluidRow(
        column(
          width = 4,
          descriptionBlock(
            header = "CHR",
            text = gene$chr,
            right_border = T,
            margin_bottom = F
          )
        ),
        column(
          width = 4,
          descriptionBlock(
            header = "BP-START",
            text = gene$`37bp1`,
            right_border = T,
            margin_bottom = F
          )
        ),
        column(
          width = 4,
          descriptionBlock(
            header = "BP-END",
            text = gene$`37bp2`,
            right_border = FALSE,
            margin_bottom = F
          )
        ),
      )
    ))
    
    output$geneLinks <- renderUI(tagList(
      ifelse(grepl("^LOC", searchString) | searchString == "TBC1D7-LOC100130357", "", tagList(div(a("NCBI Genetics Home Reference", href = paste0("https://ghr.nlm.nih.gov/gene/", searchString), target = "_blank"))))
    ))
    
    output$geneWaffle <- renderUI(tagList(
             plotOutput(
               #width = "30%",
               "aggregateDonut",
               height = "550px"
             )
    ))
    
    output$geneWaffleTable <- renderUI(tagList(
      div(
        renderDT({
          datatable(
            aggregateVariantTable,
            rownames = F,
            selection = 'none',
            options = list(
              paging = F,
              dom = 't'
            )
          ) %>% formatStyle(
            columns = "Count",
            valueColumns = "Functional consequence",
            target = 'cell',
            color = "black",
            backgroundColor = styleEqual(
              unique(c("synonymous SNV", "nonsynonymous SNV", "nonframeshift", "nonframeshift insertion", "nonframeshift deletion", "nonframeshift block substitution",  "frameshift insertion", "frameshift deletion", "frameshift block substitution", "stopgain", "stoploss", "NA/unknown")), c("#beaed4", "#386cb0", "#7fc97f", "#7fc9c9", "#a4c97f", "#5e915d", "#fdc086", "#fddd86", "#fda286", "#ffff99", "#f0027f", "#e8e6e4")
            )
          ) %>% formatStyle(columns="Functional consequence", backgroundColor = tablebgcolor(), color = tablecolor())
        }),
        id = "aggregateVariantTable")
    ))
    
    output$geneNeedle <- renderUI(tagList(
      h3('SNV-by base pair position'),
      fluidRow(
        plotlyOutput(
          "needlePlot",
          height = "400px"
        )
      )
    ))
    
    output$geneVartable <- renderUI(tagList(
      fluidRow(div(renderDT({
        dat <- variantTable[, c(1:8,11,17)]
        # dat$`Exome name (hg19)` <- str_wrap(dat$`Exome name (hg19)`, width = 10)
        datatable(
          dat,
          # plugins = "ellipsis",
          options = list(
            paging = F,
            scrollX = T,
            # scrollY = "500px",
            lengthChange = FALSE,
            # columnDefs = list(list(
            #   targets = c(3,5,6),
            #   render = JS("$.fn.dataTable.render.ellipsis( 14, false )")
            # ))
            columnDefs = list(
              #list(width = '10px', targets = 0),
              list(
                targets = c(3,5,6),
                render = JS(
                  "function(data, type, row, meta) {",
                  "return type === 'display' && data.length > 19 ?",
                  "'<span title=\"' + data + '\">' + data.substr(0, 19) + '...</span>' : data;",
                  "}")
              )
              # Maybe ask Cornelis if we can shorten Amino acid change to just the amino acid change, e.g. p.Y136Y
            )
          ),
          rownames= FALSE,
          escape = FALSE
        ) %>% formatStyle(
          columns=colnames(variantTable[, c(1:8,11,17)]),
          backgroundColor = tablebgcolor(),
          color = tablecolor()
          )
      }), style = "margin: 12px 50px 50px 12px;"))
    ))
    
    # ========== STEP 7 ==========
    
    # Show the hidden gene boxes
    show(id = "geneBoxes")
    # Animation (only the first time)
    if (geneBoxHidden) {
      startAnim(session,
                id = "geneBoxes",
                type = "slideInDown")
      geneBoxHidden <<- F
    }
    }
})