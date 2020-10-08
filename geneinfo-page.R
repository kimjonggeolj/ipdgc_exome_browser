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
    #   a. Needle Plot
    #   b. Sunburst Plot
    #   c. Aggregate table
    #   d. Variant table
    # 6. Generate and update UI
    # 7. If hidden, show the UI
    # ======================================================================
    
    
    # === Debug script ===
    #print(paste0("chromosome", chrom))
    # ====================
    
    # ========== STEP 1 ==========
    
    #load(paste0(variantFolder, "chr", chrom, ".RData"))
    initDat <- fread(paste0(variantFolder, "chr", chrom, ".tsv")) #eval(as.name(paste0("varDat.chr", chrom)))
    # === Debug scripts ===
    # print(head(initDat))
    # print("initDat loaded")
    # =====================
    
    # ========== STEP 2 ==========
    initDat <- initDat[grepl(searchString, initDat$`Gene.refGene`, ignore.case = T)]

    # ========== STEP 3 ==========
    variantTable.global <<- initDat
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
        "frameshift indel/blocksub"
      } else if (grepl("^nonframeshift", x)) {
        "nonframeshift indel/blocksub"
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
             `nonframeshift indel/blocksub` = 2,
             `frameshift indel/blocksub` = 3,
             `stoploss` = 4
      )
    })
    
    # variantTable.global <<- initDat[grepl(searchString, initDat$`Gene.refGene`, ignore.case = T)][, c("HG19_ID",
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
                                "Distribution (cases)",
                                "Distribution (control)",
                                "Distribution (proxy)",
                                "A1-frequency (cases)",
                                "A1-frequency (control)",
                                "A1-frequency (proxy)",
                                # "Exome name (hg38)",
                                "Region",
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
    # print("variantTable local loaded")
    for (i in 1:nrow(variantTable)) {
      if (nchar(variantTable$`Exome name (hg19)`[i]) > 19) {
        variantTable$`Exome name (hg19)`[i] <- paste0('<a id="', variantTable$`Exome name (hg19)`[i], '" href="javascript:;" onclick="varClick(this.id)">', gsub("^(.{20}).*$", "\\1...", variantTable$`Exome name (hg19)`[i]), '</a>')
      } else {
        variantTable$`Exome name (hg19)`[i] <- paste0('<a id="', variantTable$`Exome name (hg19)`[i], '" href="javascript:;" onclick="varClick(this.id)">', variantTable$`Exome name (hg19)`[i], '</a>')
      }
    }
    # variantTable$`Distribution (cases)` <- paste0('<div title = "homozygous alternative / heterozygous / homozygous reference">', variantTable$`Distribution (cases)`, "</div>")
    # variantTable$`Distribution (control)` <- paste0('<div title = "homozygous alternative / heterozygous / homozygous reference">', variantTable$`Distribution (control)`, "</div>")
    
    # FOR FUNCTIONAL CONSEQUENCE SUMMARY TABLE
    #     aggregate rows are currently taken from: http://annovar.openbioinformatics.org/en/latest/user-guide/gene/
    variantTable.exonic <- variantTable[variantTable$Region == "exonic"]
    aggregateVariantTable <- data.table(`Functional consequence` = c("All variants",
                                                                     "frameshift insertion",
                                                                     "frameshift deletion",
                                                                     "frameshift block substitution",
                                                                     "stopgain",
                                                                     "stoploss",
                                                                     "nonframeshift insertion",
                                                                     "nonframeshift deletion",
                                                                     "nonframeshift block substitution",
                                                                     "nonsynonymous SNV",
                                                                     "synonymous SNV",
                                                                     "nonframeshift (unknown)",
                                                                     "NA/unknown"),
                                        Count = c(nrow(variantTable.exonic),
                                                  length(which(variantTable.exonic$`Functional consequence` == "frameshift insertion")),
                                                  length(which(variantTable.exonic$`Functional consequence` == "frameshift deletion")),
                                                  length(which(variantTable.exonic$`Functional consequence` == "frameshift block substitution")),
                                                  length(which(variantTable.exonic$`Functional consequence` == "stopgain")),
                                                  length(which(variantTable.exonic$`Functional consequence` == "stoploss")),
                                                  length(which(variantTable.exonic$`Functional consequence` == "nonframeshift insertion")),
                                                  length(which(variantTable.exonic$`Functional consequence` == "nonframeshift deletion")),
                                                  length(which(variantTable.exonic$`Functional consequence` == "nonframeshift block substitution")),
                                                  length(which(variantTable.exonic$`Functional consequence` == "nonsynonymous SNV")),
                                                  length(which(variantTable.exonic$`Functional consequence` == "synonymous SNV")),
                                                  length(which(variantTable.exonic$`Functional consequence` == "nonframeshift")),
                                                  length(which(variantTable.exonic$`Functional consequence` == "unknown")) + length(which(variantTable.exonic$`Functional consequence` == "."))
                                        )
                                        )
    totalvariants <- aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "All variants"]
    nonframeshift <- c('nonsynonymous SNV', 'synonymous SNV', 'nonframeshift (unknown)', 'nonframeshift insertion', 'nonframeshift deletion', 'nonframeshift block substitution')
    aggregateVariantTable.frameshift <- aggregateVariantTable[grepl("^frameshift", aggregateVariantTable$`Functional consequence`)]
    aggregateVariantTable.nonframeshift <- aggregateVariantTable[which(`Functional consequence` %in% nonframeshift)]
    aggregateVariantTable.other <- aggregateVariantTable[which(`Functional consequence` %in% c('stopgain', 'stoploss', 'NA/unknown'))]
    # ========== STEP 5 ==========
    # Colorscale for needlePlot + waffle plot
    colorList <- list()
    colorList[[1]] <- c("synonymous SNV" = "#9bbedb", "nonsynonymous SNV" = "#87b1d4", "nonframeshift indel/blocksub" = "#377eb8", "frameshift indel/blocksub" = "#4daf4a", "stopgain" = "#e41a1c", "stoploss" = "#984ea3",  "NA/unknown" = "#e8e6e4")
    # colorList[[2]] <- c("synonymous SNV" = "#beaed4", "nonsynonymous SNV" = "#386cb0", "nonframeshift" = "#7fc97f", "nonframeshift insertion" = "#7fc9c9", "nonframeshift deletion" = "#a4c97f", "nonframeshift block substitution" = "#5e915d",  "frameshift insertion" = "#fdc086", "frameshift deletion" = "#fddd86", "frameshift block substitution" = "#fda286", "stopgain" = "#ffff99", "stoploss" = "#f0027f", "NA/unknown" = "#e8e6e4")
    # colorList[[3]] <- c(`synonymous SNV` = "#beaed4", `nonsynonymous SNV` = "#386cb0", nonframeshift = "#7fc97f", `nonframeshift insertion` = "#7fc9c9", `nonframeshift deletion` = "#a4c97f", `nonframeshift block substitution` = "#5e915d",  `frameshift insertion` = "#fdc086", `frameshift deletion` = "#fddd86", `frameshift block substitution` = "#fda286", stopgain = "#ffff99", stoploss = "#f0027f", `NA/unknown` = "#e8e6e4")
    
    # ======== UNIPROT ID =========
    # This is only possible if the positions in needle plot is in protein position

    # uniprot.id <- uniprot$uniprotID[uniprot$id == searchString]
    # proteins_acc_url <- paste0(uniprot.id, "%2C")
    # baseurl <- "https://www.ebi.ac.uk/proteins/api/features?offset=0&size=100&accession="
    # url <- paste0(baseurl,proteins_acc_url)
    # prots_feat <- GET(url,accept_json())
    # prots_feat_red <- httr::content(prots_feat)
    # features_total_plot <- NULL
    # for(i in 1:length(prots_feat_red)){ 
    #   features_temp<-drawProteins::extract_feat_acc(prots_feat_red[[i]])#the extract_feat_acc() function takes features into a data.frame
    #   features_temp$order<-i # this order is needed for plotting later
    #   features_total_plot<-rbind(features_total_plot,features_temp)
    # }
    # features_total_plot <- subset(features_total_plot, type %in% c("CHAIN","DNA_BIND","DOMAIN","MOTIF"))
    
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
          axis.text.y = element_text(color = tablecolor()
          ),
          #axis.text.y = element_blank(),
          #axis.ticks.y = element_blank(),
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
          legend.text = element_text(color = tablecolor())) +
        scale_y_continuous(
          breaks = c(0, 1, 2, 3, 4),
          labels = c("Synonymous/\nUnknown/\nNA", "Nonsynonymous", "Nonframeshift", "Frameshift", "Stopgain/loss")
        )
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
    
    # ====Waffle plot=====
    
    # output$aggregateWaffle <- renderPlot({
    #   # Waffles
    #   # How many rows do you want the y axis to have?
    #   donutDat <- aggregateVariantTable[`Functional consequence` != "All variants"]
    #   ndeep <- 10
    #   # I need to convert my data into a data.frame with uniquely-specified x
    #   # and y coordinates for each case
    #   # Note - it's actually important to specify y first for a
    #   # horizontally-accumulating waffle
    #   # One y for each row; then divide the total number of cases by the number of
    #   # rows and round up to get the appropriate number of x increments
    #   dat <- expand.grid(y = 1:ndeep,
    #                      x = seq_len(ceiling(sum(donutDat$Count) / ndeep)))
    #   
    #   # Expand the counts into a full vector of region labels - i.e., de-aggregate
    #   regionvec <- rep(donutDat$`Functional consequence`, donutDat$Count)
    #   
    #   # Depending on the value of ndeep, there might be more spots on the x-y grid
    #   # than there are cases - so fill those with NA
    #   dat$region <- c(regionvec, rep(NA, nrow(dat) - length(regionvec)))
    #   ggplot(dat, aes(x = x, y = y, fill = region)) + 
    #     geom_tile(
    #       color = tablebgcolor(),
    #       size = 2
    #     ) +
    #     # The color of the lines between tiles
    #     scale_fill_manual("Changes",
    #                       values = colorList[[2]],
    #                       guide = guide_legend(
    #                         ncol = 1
    #                       )
    #                       # breaks = colorList[[3]]0
    #     ) +
    #     labs(title = "SNV Type Distribution") +
    #     theme(
    #       axis.line=element_blank(),
    #       axis.text.x=element_blank(),
    #       axis.text.y=element_blank(),
    #       axis.ticks=element_blank(),
    #       axis.title.x=element_blank(),
    #       axis.title.y=element_blank(),
    #       panel.background=element_rect(
    #         fill = tablebgcolor(),
    #         colour = tablebgcolor()
    #       ),
    #       panel.border=element_blank(),
    #       panel.grid.major=element_blank(),
    #       panel.grid.minor=element_blank(),
    #       plot.background=element_rect(
    #         fill = tablebgcolor(),
    #         color = tablebgcolor()
    #       ),
    #       plot.title = element_text(face = "bold", hjust = 0.5, color = tablecolor()),
    #       legend.position = 'none'#'bottom',
    #       # legend.key = element_rect(
    #       #   fill = tablebgcolor(),
    #       #   color = tablebgcolor()
    #       # ),
    #       # legend.background = element_rect(fill = tablebgcolor()),
    #       # legend.title = element_blank(),
    #       # legend.text = element_text(color = tablecolor()),
    #       
    #       #,
    #       #plot.margin=grid::unit(c(0,0,0,0), "mm")
    #     )
    # })
    #========
    output$aggregateDonut <- renderPlotly({
      # DONUT
      # donutDat <- aggregateVariantTable[`Functional consequence` != "All variants"]
      #   #group_by(`Functional consequence`) %>%
      #   #summarise(count = n()) %>%
      #   plot_ly(
      #     donutDat,
      #     labels = ~`Functional consequence`,
      #     values = ~Count,
      #     type = "pie",
      #     hole = 0.6,
      #     marker = list(
      #       colors = colorList[[3]]
      #     )) %>%
      #   # add_pie(hole = 0.6
      #   #         ) %>%
      #   layout(title = "SNV Type Distribution",  showlegend = F,
      #          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      #          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      #   config(displayModeBar = F) 
      
      # SUNBURST
      sunburst.elements <- list()
      # Element generation
      # 1. All variants
      sunburst.elements[[1]] <- list("All variants")
      sunburst.elements[[2]] <- list("")
      sunburst.elements[[3]]<- list(aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "All variants"])
      
      frameshift.count <- 0
      nonframeshift.count <- 0
      
      # 2a. frameshift insertion
      if (0 < aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "frameshift insertion"]) {
        frameshift.count <- aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "frameshift insertion"]
        sunburst.elements[[1]] <- append(sunburst.elements[[1]], "insertion ")
        sunburst.elements[[2]] <- append(sunburst.elements[[2]], "frameshift")
        sunburst.elements[[3]] <- append(sunburst.elements[[3]], aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "frameshift insertion"])
      }
      # 2b. frameshift deletion
      if (0 < aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "frameshift deletion"]) {
        frameshift.count <- frameshift.count + aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "frameshift deletion"]
        sunburst.elements[[1]] <- append(sunburst.elements[[1]], "deletion ")
        sunburst.elements[[2]] <- append(sunburst.elements[[2]], "frameshift")
        sunburst.elements[[3]] <- append(sunburst.elements[[3]], aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "frameshift deletion"])
      }
      # 2c. frameshift block substitution
      if (0 < aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "frameshift block substitution"]) {
        frameshift.count <- frameshift.count + aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "frameshift block substitution"]
        sunburst.elements[[1]] <- append(sunburst.elements[[1]], "block substitution ")
        sunburst.elements[[2]] <- append(sunburst.elements[[2]], "frameshift")
        sunburst.elements[[3]] <- append(sunburst.elements[[3]], aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "frameshift block substitution"])
      }
      # 2d. frameshift
      if (0 < frameshift.count) {
        sunburst.elements[[1]] <- append(sunburst.elements[[1]], "frameshift")
        sunburst.elements[[2]] <- append(sunburst.elements[[2]], "All variants")
        sunburst.elements[[3]] <- append(sunburst.elements[[3]], frameshift.count)
      }
      # 3a. nonframeshift insertion
      if (0 < aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "nonframeshift insertion"]) {
        nonframeshift.count <- aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "nonframeshift insertion"]
        sunburst.elements[[1]] <- append(sunburst.elements[[1]], "insertion")
        sunburst.elements[[2]] <- append(sunburst.elements[[2]], "nonframeshift")
        sunburst.elements[[3]] <- append(sunburst.elements[[3]], aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "nonframeshift insertion"])
      }
      # 3b. nonframeshift deletion
      if (0 < aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "nonframeshift deletion"]) {
        nonframeshift.count <- nonframeshift.count + aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "nonframeshift deletion"]
        sunburst.elements[[1]] <- append(sunburst.elements[[1]], "deletion")
        sunburst.elements[[2]] <- append(sunburst.elements[[2]], "nonframeshift")
        sunburst.elements[[3]] <- append(sunburst.elements[[3]], aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "nonframeshift deletion"])
      }
      # 3c. nonframeshift block substitution
      if (0 < aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "nonframeshift block substitution"]) {
        nonframeshift.count <- nonframeshift.count + aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "nonframeshift block substitution"]
        sunburst.elements[[1]] <- append(sunburst.elements[[1]], "block substitution")
        sunburst.elements[[2]] <- append(sunburst.elements[[2]], "nonframeshift")
        sunburst.elements[[3]] <- append(sunburst.elements[[3]], aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "nonframeshift block substitution"])
      }
      # 3d. nonsynonymous SNV
      # check if any exist
      if (0 < aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "nonsynonymous SNV"]) {
        nonframeshift.count <- nonframeshift.count + aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "nonsynonymous SNV"]
        sunburst.elements[[1]] <- append(sunburst.elements[[1]], "nonsynonymous SNV (missense)")
        sunburst.elements[[2]] <- append(sunburst.elements[[2]], "nonframeshift")
        sunburst.elements[[3]] <- append(sunburst.elements[[3]], aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "nonsynonymous SNV"])
      }
      # 3e. synonymous SNV
      # check if any exist
      if (0 < aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "nonsynonymous SNV"]) {
        nonframeshift.count <- nonframeshift.count + aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "synonymous SNV"]
        sunburst.elements[[1]] <- append(sunburst.elements[[1]], "synonymous SNV")
        sunburst.elements[[2]] <- append(sunburst.elements[[2]], "nonframeshift")
        sunburst.elements[[3]] <- append(sunburst.elements[[3]], aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "synonymous SNV"])
      }
      # 3e. nonframeshift
      if (0 < aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "nonframeshift (unknown)"]) {
        nonframeshift.count <- nonframeshift.count + aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "nonframeshift"]
        sunburst.elements[[1]] <- append(sunburst.elements[[1]], "unknown")
        sunburst.elements[[2]] <- append(sunburst.elements[[2]], "nonframeshift")
        sunburst.elements[[3]] <- append(sunburst.elements[[3]], aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "nonframeshift (unknown)"])
      }
      # 3f. nonframeshift (whole category)
      if (0 < nonframeshift.count) {
        sunburst.elements[[1]] <- append(sunburst.elements[[1]], "nonframeshift")
        sunburst.elements[[2]] <- append(sunburst.elements[[2]], "All variants")
        sunburst.elements[[3]] <- append(sunburst.elements[[3]], nonframeshift.count)
      }
      # 4. NA/Unknown
      if (0 < aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "NA/unknown"]) {
        sunburst.elements[[1]] <- append(sunburst.elements[[1]], "NA/unknown")
        sunburst.elements[[2]] <- append(sunburst.elements[[2]], "All variants")
        sunburst.elements[[3]] <- append(sunburst.elements[[3]], aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "NA/unknown"])
      }
      # 5. stop gain
      if (0 < aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "stopgain"]) {
        sunburst.elements[[1]] <- append(sunburst.elements[[1]], "stopgain")
        sunburst.elements[[2]] <- append(sunburst.elements[[2]], "All variants")
        sunburst.elements[[3]] <- append(sunburst.elements[[3]], aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "stopgain"])
      }
      # 6. stop loss
      if (0 < aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "stoploss"]) {
        sunburst.elements[[1]] <- append(sunburst.elements[[1]], "stoploss")
        sunburst.elements[[2]] <- append(sunburst.elements[[2]], "All variants")
        sunburst.elements[[3]] <- append(sunburst.elements[[3]], aggregateVariantTable$Count[aggregateVariantTable$`Functional consequence` == "stoploss"])
      }
      # 7. colors
      sunburst.elements[[4]] <- sunburst.elements[[1]]
      sunburst.elements[[4]] <- gsub("All variants", "", sunburst.elements[[4]])
      sunburst.elements[[4]] <- gsub("NA/unknown", tablebgcolor(), sunburst.elements[[4]])
      sunburst.elements[[4]] <- gsub("insertion ", "#5eb75c", sunburst.elements[[4]])
      sunburst.elements[[4]] <- gsub("deletion ", "#70bf6e", sunburst.elements[[4]])
      sunburst.elements[[4]] <- gsub("block substitution ", "#82c780", sunburst.elements[[4]])
      sunburst.elements[[4]] <- gsub("insertion", "#4b8abf", sunburst.elements[[4]])
      sunburst.elements[[4]] <- gsub("deletion", "#5e97c6", sunburst.elements[[4]])
      sunburst.elements[[4]] <- gsub("block substitution", "#73a4cd", sunburst.elements[[4]])
      sunburst.elements[[4]] <- gsub("nonsynonymous SNV", "#87b1d4", sunburst.elements[[4]])
      sunburst.elements[[4]] <- gsub("synonymous SNV", "#9bbedb", sunburst.elements[[4]])
      sunburst.elements[[4]] <- gsub("unknown", "#afcbe2", sunburst.elements[[4]])
      sunburst.elements[[4]] <- gsub("nonframeshift", "#377eb8", sunburst.elements[[4]])
      sunburst.elements[[4]] <- gsub("frameshift", "#4daf4a", sunburst.elements[[4]])
      sunburst.elements[[4]] <- gsub("stopgain", "#e41a1c", sunburst.elements[[4]])
      sunburst.elements[[4]] <- gsub("stoploss", "#984ea3", sunburst.elements[[4]])
      
      plot_ly(
        labels = as.character(sunburst.elements[[1]]),
        parents = as.character(sunburst.elements[[2]]),
        values = as.numeric(sunburst.elements[[3]]),
        type = 'sunburst',
        branchvalues = 'total',
        marker = list(colors = sunburst.elements[[4]])
      ) %>% 
        layout(
          title = "Exonic Variant Distribution by Functional Consequence",
          font = list(color=tablecolor()),
          plot_bgcolor=tablebgcolor(),
          paper_bgcolor=tablebgcolor())
    })
    
    # SHARE button output
    output$geneShareLink <- renderText({
      paste0(
        session$clientData$url_protocol,
        "//",
        session$clientData$url_hostname,
        session$clientData$url_port,
        session$clientData$url_pathname,
        "?gene=",
      #"https://pdgenetics.shinyapps.io/LRRK2Browser/?gene=",
      searchString)})
    
    
    # ========== STEP 6 ==========
    
    output$geneInfo <- renderUI(tagList(
      h1(
        searchString,
        tags$sup(
          actionLink(
            "geneShareButton",
            icon = icon("link"),
            label = ""
          )
        ),
        hidden(
          absolutePanel(
            id = "geneSharePanel",
            verbatimTextOutput("geneShareLink")
          )
        )
        ),
      #gene$id),
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
      ifelse(
        grepl("^LOC", searchString) | searchString == "TBC1D7-LOC100130357",
        "",
        tagList(
          div(
            a(
              "NCBI Genetics Home Reference",
              href = paste0("https://ghr.nlm.nih.gov/gene/", searchString),
              target = "_blank"
              ),
            "|",
            a(
              "gnomAD",
              href = paste0("https://gnomad.broadinstitute.org/gene/", searchString),
              target = "_blank"
            ),
            "|",
            a(
              "GeneCards",
              href = paste0("https://www.genecards.org/cgi-bin/carddisp.pl?gene=", searchString),
              target = "_blank"
            ),
            "|",
            a(
              "GTEx",
              href = paste0("https://www.gtexportal.org/home/gene/", searchString),
              target = "_blank"
            )
            )
          )
        )
    ))
    
    output$geneWaffle <- renderUI(tagList(
      plotlyOutput(
        "aggregateDonut",
        height = "550px",
        width = "95%"
      )#,
      # plotOutput(
      #   #width = "30%",
      #   "aggregateWaffle",
      #   height = "550px"
      # )
    ))
    
    output$geneWaffleTable <- renderUI(tagList(
      div(
        h3("Number of all exonic variants:", totalvariants),
        column(
          width = 4,
          renderDT({
            datatable(
              aggregateVariantTable.other,
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
                c("stopgain", "stoploss", "NA/unknown"),
                c("#e41a1c", "#984ea3", "#e8e6e4"                                                                                                                                                                                                                                                                          )
              )
            ) %>% formatStyle(columns="Functional consequence", backgroundColor = tablebgcolor(), color = tablecolor())
          })
        ),
        column(
          width = 4,
          renderDT({
          datatable(
            aggregateVariantTable.frameshift,
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
              c("frameshift insertion", "frameshift deletion", "frameshift block substitution"),
              c("#5eb75c", "#70bf6e", "#82c780"                                                                                                                                                                                                                                                                         )
            )
          ) %>% formatStyle(columns="Functional consequence", backgroundColor = tablebgcolor(), color = tablecolor())
        })
        ),
        column(
          width = 4,renderDT({
          datatable(
            aggregateVariantTable.nonframeshift,
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
              c("synonymous SNV", "nonsynonymous SNV", "nonframeshift (unknown)", "nonframeshift insertion", "nonframeshift deletion", "nonframeshift block substitution"),
              c("#9bbedb", "#87b1d4", "#afcbe2", "#4b8abf", "#5e97c6", "#73a4cd"                                                                                                                                                                                                                                                                    )
            )
          ) %>% formatStyle(columns="Functional consequence", backgroundColor = tablebgcolor(), color = tablecolor())
        })
        )
        #id = "aggregateVariantTable"
        )
    ))
    
    output$geneNeedle <- renderUI(tagList(
      h3('Variants by base pair position'),
      fluidRow(
        plotlyOutput(
          "needlePlot",
          height = "400px",
          width = "95%"
        )
      )
    ))
    
    output$geneVartable <- renderUI(tagList(
      fluidRow(
        div(
          radioGroupButtons(
            inputId = "vartable.filter",
            label = div("Filter variants:",
                        title = "For more options, please use the \"Search\" box on the right!"),
            choices = c("No filter",
                        "Nonsynonymous",
                        "Frameshift",
                        "Stop gain/loss"),
            selected = "No filter",
            status = "primary",
            checkIcon = list(
              yes = icon("ok", 
                         lib = "glyphicon"),
              no = icon("remove",
                        lib = "glyphicon"))
          ),
          renderDT({
            dat <- variantTable[, c(1,5:8,10:12,14,15)] #c(1:8,11,17)]
            dat$`Amino acid change` <- gsub(".*(p\\..*)", "\\1", dat$`Amino acid change`)
            # vartable filter
            if (!is.na(input$vartable.filter)) {
              dat <- switch(input$vartable.filter,
                            "No filter" = dat,
                            "Nonsynonymous" = dat[dat$`Functional consequence` %ni% c(".", "synonymous SNV")],
                            "Frameshift" = dat[dat$`Functional consequence` %ni% c("frameshift deletion", "frameshift insertion", "frameshift block substitution")],
                            "Stop gain/loss" = dat[dat$`Functional consequence` %in% c("stopgain", "stoploss")]
              )
            }
            # dat$`Exome name (hg19)` <- str_wrap(dat$`Exome name (hg19)`, width = 10)
            datatable(
              dat,
              extensions = 'Buttons',
              # plugins = "ellipsis",
              options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
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
                    targets = c(4:8),
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 19 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 19) + '...</span>' : data;",
                      "}"
                    )
                  )
                )
              ),
              rownames= FALSE,
              escape = FALSE
            ) %>% formatStyle(
              columns=colnames(variantTable[, c(1,5:8,10:12,14,15)]),#variantTable[, c(1:8,11,17)]),
              backgroundColor = tablebgcolor(),
              color = tablecolor()
            )
          }),
          style = "margin: 12px 50px 50px 12px;"
        )
      )
    ))
    
    # ========== STEP 7 ==========
    
    # Show the hidden gene boxes
    show(id = "geneBoxes")
    # Animation (only the first time)
    # if (geneBoxHidden) {
    #   startAnim(session,
    #             id = "geneBoxes",
    #             type = "slideInDown")
    #   geneBoxHidden <<- F
    # }
    
    # ========== Tutorial =============
    # Tutorial
    tutorial <<- "gene"
    if (input$tutorial.mode == T) {
      output$tutorial <- tutorial.gene
    }
    }
})

observeEvent(input$geneShareButton,
             {
               toggle(id = "geneSharePanel")
             }
             )