#====MAIN function for search
# currently accepted input formats:
# 1. Gene ID (default)
searchSwitch <- "geneID"
# 2. chromosome number (chrXX)
# 3. chromosome number + base pair (XX:YYYYYYYY)
# 4. chromosome number + base pair range (XX:YYYYYYY-ZZZZZZZ)
# NEED TO ADD: rsID (may be a huge headache due to excessive number of rsIDs)
# Maybe gene name????
searchFunction <- function (searchString = input$searchBar, type) {
  # this is setting up for base pair range search
  ranges <- data.table(`37bp1` = as.numeric(gsub("^\\d+:(\\d+)-\\d+$", "\\1", searchString, ignore.case = T)), `37bp2` = as.numeric(gsub("^\\d+:\\d+-(\\d+)$", "\\1", searchString, ignore.case = T)))
  setkey(ranges, `37bp1`, `37bp2`)
  preOverlap <- geneList[grepl(gsub("^(\\d+):.*$", "\\1",searchString, ignore.case = T),
        geneList$chr,
        ignore.case = T)]
  switch(type,
         geneID = geneList[grepl(searchString, geneList$id, ignore.case = T)],
         chr = geneList[grepl(gsub("^chr(\\d+)", "\\1", searchString, ignore.case = T), geneList$chr, ignore.case = T)],
         chrbp = subset( #first subset by chromosome number
           subset(geneList,
                  grepl(gsub("^(\\d+):.*$", "\\1",searchString, ignore.case = T),
                        geneList$chr,
                        ignore.case = T)
           ),
           # then see if provided bp is in any bp range of the genes
           inrange(
             as.numeric(gsub("^\\d+:(\\d+)$", "\\1", searchString, ignore.case = T)),
             geneList$`37bp1`,
             geneList$`37bp2`
           )
         ),
         # look for overlap between provided bp ranges
         # add chromosome at some point?????
         chrbpRange = foverlaps(preOverlap, ranges, nomatch = NULL)[,c(3:9)]
  )
}

#====Function for running the search
runSearchPage <- function() {
  # detecting switch for the function
  if (grepl("^chr\\d+$", input$searchBar, ignore.case = T)) {
    searchSwitch <- "chr"
  } else if (grepl("^\\d+:\\d*$", input$searchBar, ignore.case = T)){
    searchSwitch <- "chrbp"
  } else if (grepl("^\\d{1,2}:\\d+-\\d+$", input$searchBar, ignore.case = T)) {
    searchSwitch <- "chrbpRange"
  }
  # if initial main page, then read from big search bar
  # else read from mini search bar
  searchSelect <- if (pageState == 1) {
    input$searchBar
  } else if (pageState == 2 | 3 | 4) {
    input$minisearchBar
  }
  
  # run search, set names of columns, then pass the result to storedRes for display in gene info
  res <- searchFunction(
    searchString = searchSelect,  type = searchSwitch)[,c(1:3,6,7)]
  colnames(res) <- c("id", "name", "chr", "37bp1", "37bp2")
  storedRes <<- res
  # prepping result page
  #   below segment specifically sets up the geneID elements to have
  #   a javascript function associated with it on click. On click, it
  #   should open the gene information page. "See clickdetect.js"
  #   for details.
  for (i in 1:nrow(res)) {
    res$id[i] <- paste0('<a id="res', i, '" href="#" class="action-button shiny-bound-input" onclick="resClick(this.id)">', res$id[i], '</a>')
    restoreInput(id = paste0("res", i), default = NULL)
  }
  
  if (input$darktheme == T) {
    tablebgcolor <- '#282828'
    tablecolor <- "white"
  } else {
    tablebgcolor <- '#FFFFFF'
    tablecolor <- "black"
  }
  
  colnames(res) <- c("Gene ID", "Gene Name", "Chromosome", "BP-Start", "BP-End")
  # UI rending of serach results
  resultPage <<- renderUI(tagList(
    fluidRow(
      renderDT({datatable(res, escape = FALSE, rownames= FALSE) %>%
          formatStyle(columns=colnames(res),
                      backgroundColor = tablebgcolor, color = tablecolor)})
    ),
    # load the javascript
    tags$script(src = "clickdetect.js")
  ))
  
  output$mainPage <- resultPage
  
  #show hidden logo
  show(id = "wrapperlogo")
  show(id = "miniSearchBar")
  show(id = "minisubmit")
  pageState <<- 2
}

# initiates search function on hitting the submit button
observeEvent(
  input$submit,
  runSearchPage()
)

# initiates search function on hitting the minisubmit button
observeEvent(
    input$minisubmit,
    runSearchPage()
  )



