#====MAIN function for search
# currently accepted input formats:
# 1. Gene ID (default)
searchSwitch <- "geneID"
# 2. chromosome number (chrXX)
# 3. chromosome number + base pair (XX:YYYYYYYY)
# 4. chromosome number + base pair range (XX:YYYYYYY-ZZZZZZZ)
# NEED TO ADD: rsID (may be a huge headache due to excessive number of rsIDs)
# Maybe gene name????
# eventReactive(
#   searchString,
#   {
#     if (input$buildSwitch == F) {
#       ranges <- data.table(`37bp1` = as.numeric(gsub("^\\d+:(\\d+)-\\d+$", "\\1", searchString, ignore.case = T)), `37bp2` = as.numeric(gsub("^\\d+:\\d+-(\\d+)$", "\\1", searchString, ignore.case = T)))
#       setkey(ranges, `37bp1`, `37bp2`)
#     } else {
#       ranges <- data.table(`38bp1` = as.numeric(gsub("^\\d+:(\\d+)-\\d+$", "\\1", searchString, ignore.case = T)), `38bp2` = as.numeric(gsub("^\\d+:\\d+-(\\d+)$", "\\1", searchString, ignore.case = T)))
#       setkey(ranges, `38bp1`, `38bp2`)
#     }
#   }
# )

searchFunction <- function (searchGene, searchString = input$searchBar, type) {
  # this is setting up for base pair range search
  dat <- if(searchGene) {geneList} else {varList}
  preOverlap <- dat[grepl(gsub("^(\\d+):.*$", "\\1",searchString, ignore.case = T),
        dat$chr,
        ignore.case = T)]
  if (input$buildSwitch == F) {
    ranges <- data.table(`37bp1` = as.numeric(gsub("^\\d+:(\\d+)-\\d+$", "\\1", searchString, ignore.case = T)), `37bp2` = as.numeric(gsub("^\\d+:\\d+-(\\d+)$", "\\1", searchString, ignore.case = T)))
    setkey(ranges, `37bp1`, `37bp2`)
  } else {
    ranges <- data.table(`38bp1` = as.numeric(gsub("^\\d+:(\\d+)-\\d+$", "\\1", searchString, ignore.case = T)), `38bp2` = as.numeric(gsub("^\\d+:\\d+-(\\d+)$", "\\1", searchString, ignore.case = T)))
    setkey(ranges, `38bp1`, `38bp2`)
  }
  switch(type,
         geneID = dat[grepl(searchString, dat$id, ignore.case = T)],
         chr = dat[grepl(gsub("^chr(\\d+)", "\\1", searchString, ignore.case = T), dat$chr, ignore.case = T)],
         chrbp37 = subset( #first subset by chromosome number
           subset(dat,
                  grepl(gsub("^(\\d+):.*$", "\\1",searchString, ignore.case = T),
                        dat$chr,
                        ignore.case = T)
           ),
           # then see if provided bp is in any bp range of the genes
           inrange(
             as.numeric(gsub("^\\d+:(\\d+)$", "\\1", searchString, ignore.case = T)),
             as.numeric(dat$`37bp1`)-1000,
             as.numeric(dat$`37bp2`)+1000
           )
         ),
         chrbp38 = subset( #first subset by chromosome number
           subset(dat,
                  grepl(gsub("^(\\d+):.*$", "\\1",searchString, ignore.case = T),
                        dat$chr,
                        ignore.case = T)
           ),
           # then see if provided bp is in any bp range of the genes
           inrange(
             as.numeric(gsub("^\\d+:(\\d+)$", "\\1", searchString, ignore.case = T)),
             dat$`38bp1`,
             dat$`38bp2`
           )
         ),
         # look for overlap between provided bp ranges
         # add chromosome at some point?????
         chrbpRange = foverlaps(preOverlap, ranges, nomatch = NULL)[,c(3:9)],
         rsID = dat[grepl(searchString, dat$rsID, ignore.case = T)]
  )
}

#====Function for running the search
runSearchPage <- function() {
  # if initial main page, then read from big search bar
  # else read from mini search bar
  searchSelect <- input$minisearchBar
  listSwitch <- !grepl("^rs\\d*", searchSelect, ignore.case = T)
  # detecting switch for the function
  if (grepl("^chr\\d+$", searchSelect, ignore.case = T)) {
    searchSwitch <- "chr"
  } else if (grepl("^\\d+:\\d*$", searchSelect, ignore.case = T)){
    if (input$buildSwitch == F){
      searchSwitch <- "chrbp37"
    } else {
      searchSwitch <- "chrbp38"
    }
  } else if (grepl("^\\d{1,2}:\\d+-\\d+$", searchSelect, ignore.case = T)) {
    searchSwitch <- "chrbpRange"
  } else if (grepl("^rs\\d*", searchSelect, ignore.case = T)) {
    searchSwitch <- "rsID"
    #listSwitch <- F
  }
  
  # run search, set names of columns, then pass the result to storedRes for display in gene info
  res <- searchFunction(searchGene = listSwitch,
    searchString = searchSelect,  type = searchSwitch)
  if (listSwitch) {
    if (input$buildSwitch == F) {
      res <- res[,c(1,3,6,7,2)]
      # colnames(res) <- c("id", "name", "chr", "37bp1", "37bp2")
    } else {
      res <- res[,c(1,3:5,2)]
      # colnames(res) <- c("id", "name", "chr", "37bp1", "37bp2")
    }
    #storedRes <<- res
    # for (i in 1:nrow(res)) {
    #   res$id[i] <- paste0('<a id="', res$id[i], '" href="#" onclick="resClick(this.id)">', res$id[i], '</a>')
    # }
    colnames(res) <- c("Gene ID", "Chromosome", "BP-Start", "BP-End", "Gene Name")
  } else {
    #storedRes <<- res
    colnames(res) <- c("Position (Ref/Alt)", "rsID", "Gene ID")
    for (i in 1:nrow(res)) {
      res$`Gene ID`[i] <- paste0('<a id="', res$`Gene ID`[i], '" href="#" onclick="resClick(this.id)">', res$`Gene ID`[i], '</a>')
      res$`Position (Ref/Alt)`[i] <- paste0('<a id="', res$`Position (Ref/Alt)`[i], '" href="#" onclick="varClick(this.id)">', res$`Position (Ref/Alt)`[i], '</a>')
    }
  }
  
  # prepping result page
  #   below segment specifically sets up the geneID elements to have
  #   a javascript function associated with it on click. On click, it
  #   should open the gene information page. "See clickdetect.js"
  #   for details.

  
  resultTable <- renderDT(
    {
      datatable(
        res,
        # options = list(
        #   autoWidth = TRUE,
        #   columnDefs = list(list(width = '10%', targets = c(1, 3)))
        # ),
        escape = FALSE,
        rownames= FALSE,
        selection = 'none'
        ) %>%
      formatStyle(
        columns=colnames(res),
        style="bootstrap",
        backgroundColor = tablebgcolor(),
        color = tablecolor()
        )
      }
    )
  

  # UI rending of search results
  output$panel1 <<- renderUI(tagList(
    h4(
      "Current build:",
      ifelse(
        input$buildSwitch,
        "hg38",
        "hg19"
        )
      ),
    resultTable
  )
  )


}


# initiates search function on hitting the minisubmit button
observeEvent(
    input$minisearchBar_search,
    runSearchPage()
  )

observeEvent(
  input$buildSwitch,
  runSearchPage()
)

# observeEvent(
#   input$buildSwitch,
#   {
#     val <- input$minisearchBar
#     updateSearchInput(
#       session,
#       inputId = "minisearchBar",
#       value = ""
#     )
#     # updateSearchInput(
#     #   session,
#     #   inputId = "minisearchBar",
#     #   value = val
#     # )
#   }
# )
