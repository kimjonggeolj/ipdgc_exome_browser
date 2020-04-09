output$resultArrowUI <- renderUI({
  actionLink(
    "resultArrow",
    HTML("&nbsp;"),
    icon = if (resultHidden) {
      icon("chevron-down")
    } else {
      icon("chevron-up")
    }
  )
})

#====MAIN function for search
# currently accepted input formats:
# 1. Gene ID (default)
# 2. chromosome number (chrXX)
# 3. chromosome number + base pair (XX:YYYYYYYY)
# 4. chromosome number + base pair range (XX:YYYYYYY-ZZZZZZZ)
# 5. rsID
# Maybe gene name????

# ==hg19 vs 38==
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
  dat.nested <- list()
  if (type == "chrbpRange") {
    ranges <- data.table(`37bp1` = as.numeric(gsub("^\\d+:(\\d+)-\\d+$", "\\1", searchString, ignore.case = T)), `37bp2` = as.numeric(gsub("^\\d+:\\d+-(\\d+)$", "\\1", searchString, ignore.case = T)))
    setkey(ranges, `37bp1`, `37bp2`)
  }
  if (type %ni% c("rsID", "geneID", "STOP")) {
    chrom <- gsub("^(\\d+):.*$", "\\1", searchString, ignore.case = T)
    dat.nested[[1]] <- geneList[grepl(chrom, geneList$chr)]
    # vroom/tibble method
    if (type == "chr") {
      #=== note the following line was added here to fix a bug. When the function took in type as `chr`,
      #    it would set variable `chrom` as the whole search string. This would crash the app since the
      #    vroom function would not be able to find the `chrom`.tsv.
      dat.nested[[2]] <- data.table(id = character(), geneID = character(), rsID = character(), chr = character(), `37bp1` = character())
    } else {
      dat.nested[[2]] <- vroom(paste0("data/searchLists/varList/", chrom, ".tsv"))
    }
    # data.table method
    #varList.nested[[chrom]]
  }
  switch(type,
         STOP = {
           # empty
           dat.nested[[1]] <- geneList[geneList$chr == "A"]
           # empty
           # vroom/tibble
           dat.nested[[2]] <- data.table(id = character(), geneID = character(), rsID = character(), chr = character(), `37bp1` = character())
           # data.table method
           #dat.nested[[2]] <- varList.nested[[1]][0]
         },
         rsID = {
           rsFirst <- gsub("^(rs\\d\\d)\\d+$", "\\1", searchString, ignore.case = F)
           # vroom/tibble version
           dat.var <- vroom(paste0("data/searchLists/rsID/", rsFirst, ".tsv"))
           dat.nested[[2]] <- as.data.table(filter(dat.var, grepl(searchString, rsID)))
           # data.table with nested list
           # dat.Var <- varList.rsID.nested[[rsFirst]]
           # dat.nested[[2]] <- dat.Var[grepl(searchString, dat.Var$rsID)]
           # empty
           dat.nested[[1]] <- geneList[geneList$chr == "A"]
         },
         chr = {
           dat.nested[[1]] <- geneList[grepl(gsub("^chr(\\d+)", "\\1", searchString, ignore.case = T), geneList$chr, ignore.case = T)]
           # empty
           # vroom/tibble
           #=== Note: the next line is commented out because it was copied up in `type == "chr" line above (line 50 as of 2020/04/09)`
           # dat.nested[[2]] <- data.table(id = character(), geneID = character(), rsID = character(), chr = character(), `37bp1` = character())
           # data.table
           # dat.nested[[2]] <- varList.nested[[1]][0]
         },
         chrbp37 = {
           bp <- as.numeric(gsub("^\\d+:(\\d+).*", "\\1", searchString, ignore.case = T))
           dat.nested[[1]] <- dat.nested[[1]][bp >= dat.nested[[1]]$`37bp1` & bp <= dat.nested[[1]]$`37bp2`] # then see if provided bp is in any bp range of the genes
           # vroom/tibble
           dat.nested[[2]] <- as.data.table(dat.nested[[2]] %>% filter(`37bp1` == bp))
           # data.table
           # dat.nested[[2]] <- dat.nested[[2]][bp >= dat.nested[[2]]$`37bp1` & bp <= dat.nested[[2]]$`37bp2`]
         },
         chrbp38 = {
           bp <- as.numeric(gsub("^\\d+:(\\d+).*", "\\1", searchString, ignore.case = T))
           dat.nested[[1]] <- dat.nested[[1]][bp >= dat.nested[[1]]$`38bp1` & bp <= dat.nested[[1]]$`38bp2`] # then see if provided bp is in any bp range of the genes
           dat.nested[[2]] <- dat.nested[bp >= dat.nested$`38bp1` & bp <= dat.nested$`38bp2`]
         },
         chrbpRange = {
           dat.nested[[1]] <- foverlaps(dat.nested[[1]], ranges, nomatch = NULL)[,c(3:9)]
           # this is rearranged later; this formatting is to make it consistent with other results
           dat.nested[[1]] <- dat.nested[[1]][, c("id", "name", "chr", "38bp1", "38bp2", "i.37bp1", "i.37bp2")]
           # vroom/tibble
           dat.nested[[2]] <- as.data.table(dat.nested[[2]] %>% filter(bp %in% gsub("^\\d+:(\\d+)-\\d+$", "\\1", searchString, ignore.case = T):gsub("^\\d+:\\d+-(\\d+)$", "\\1", searchString, ignore.case = T)))
           # data.table
           #dat.nested[[2]] <- foverlaps(dat.nested[[2]], ranges, nomatch = NULL)[,c(3:8)]
           #dat.nested[[2]] <- dat.nested[[2]][, c("id", "chr", "i.37bp1", "i.37bp2", "geneID", "rsID")]
         },
         geneID = {
           dat.nested[[1]] <- geneList[grepl(searchString, geneList$id, ignore.case = T)]
           # vroom/tibble
           dat.nested[[2]] <- data.table(id = character(), geneID = character(), rsID = character(), chr = character(), `37bp1` = character())
           # data.table
           # dat.nested[[2]] <- varList.nested[[1]][0]
         }
  )
  
  # prepping result page
  #   below segment specifically sets up the geneID elements to have
  #   a javascript function associated with it on click. On click, it
  #   should open the gene information page. "See clickdetect.js"
  #   for details.
  
  #if (nrow(dat.nested[[2]]) <= 1000) {
    for (i in 1:nrow(dat.nested[[2]])) {
      dat.nested[[2]]$id[i] <- paste0('<a id="', dat.nested[[2]]$id[i], '" href="javascript:;"onclick="varResClick(this.id)">', dat.nested[[2]]$id[i], '</a>')
      dat.nested[[2]]$geneID[i] <- paste0('<a id="', dat.nested[[2]]$geneID[i], '" href="javascript:;"onclick="resClick(this.id)">', dat.nested[[2]]$geneID[i], '</a>')
    }
 # }
  dat.nested
}

#====Function for running the search
runSearchPage <- function() {
  # if URL, then read from URL
  # else read from mini search bar
  if (runFromURL) {
    if (!is.null(query[['variant']])) {
      searchSelect <- query[['variant']]
    } else {
      searchSelect <- query[['gene']]
    }
    # runFromURL <<- F
  } else {
    searchSelect <- input$minisearchBar
  }
  searchSelect <- gsub("\\s", "", searchSelect)
  #listSwitch <- !grepl("^rs\\d*", searchSelect, ignore.case = T)
  # detecting switch for the function
  if (grepl("^chr\\d+$", searchSelect, ignore.case = T)) {
    searchSwitch <- "chr"
  } else if (grepl("^\\d{1,2}:\\d+-\\d+$", searchSelect, ignore.case = T)) {
    searchSwitch <- "chrbpRange"
  } else if (grepl("^\\d{1,2}:\\d+.*", searchSelect, ignore.case = T)){
    searchSwitch <- "chrbp37"
    # if (input$buildSwitch == F){
    #   searchSwitch <- "chrbp37"
    # } else {
    #   searchSwitch <- "chrbp38"
    # }
  } else if (grepl("^rs\\d*", searchSelect, ignore.case = T)) {
    if (grepl("^rs\\d{1,2}$", searchSelect, ignore.case = T)) {
      # throw error if rsID is < 4 digits long
      sendSweetAlert(
        session,
        title = "Search field error!",
        text = 'Please provide at least three digits in the rsID.',
        type =  "error"
      )
      searchSwitch <- "STOP"
    } else {
      searchSwitch <- "rsID"
    }
    #listSwitch <- F
  } else {
    searchSwitch <- "geneID"
  }
  
  # run search, set names of columns, then pass the result to storedRes for display in gene info
  res <- searchFunction(searchGene = listSwitch,
    searchString = searchSelect, type = searchSwitch)
  res[[1]] <- res[[1]][, c(1,3,6,7,2)]
  colnames(res[[1]]) <- c("Gene ID", "Chromosome", "BP-Start", "BP-End", "Gene Name")
  # == hg19 vs 38==
  # if (listSwitch) {
  #   # if (input$buildSwitch == F) {
  #   #   res <- res[,c(1,3,6,7,2)]
  #   #   # colnames(res) <- c("id", "name", "chr", "37bp1", "37bp2")
  #   # } else {
  #   #   res <- res[,c(1,3:5,2)]
  #   #   # colnames(res) <- c("id", "name", "chr", "37bp1", "37bp2")
  #   # }
  #   #storedRes <<- res
  #   # for (i in 1:nrow(res)) {
  #   #   res$id[i] <- paste0('<a id="', res$id[i], '" href="#" onclick="resClick(this.id)">', res$id[i], '</a>')
  #   # }
  #   colnames(res) <- c("Gene ID", "Chromosome", "BP-Start", "BP-End", "Gene Name")
  # } else {
  #   #storedRes <<- res
  #   colnames(res) <- c("Position (Ref/Alt)", "rsID", "Gene ID")
  #   for (i in 1:nrow(res)) {
  #     res$`Gene ID`[i] <- paste0('<a id="', res$`Gene ID`[i], '" href="#" onclick="resClick(this.id)">', res$`Gene ID`[i], '</a>')
  #     res$`Position (Ref/Alt)`[i] <- paste0('<a id="', res$`Position (Ref/Alt)`[i], '" href="#" onclick="varClick(this.id)">', res$`Position (Ref/Alt)`[i], '</a>')
  #   }
  # }
  
  #varRes <- searchFunctionVar(searchString = searchSelect,  type = searchSwitch)
  colnames(res[[2]]) <- c("Position (Ref/Alt)", "Nearest Gene ID", "rsID", "Chromosome", "BP")
  # for (i in 1:nrow(res[[2]])) {
  #   res[[2]]$`Position (Ref/Alt)`[i] <- paste0('<a id="', res[[2]]$`Position (Ref/Alt)`[i], '" href="#" onclick="varResClick(this.id)">', res[[2]]$`Position (Ref/Alt)`[i], '</a>')
  # }


  
  resultTable <- renderDT(
    {
      datatable(
        res[[1]],
        # options = list(
        #   autoWidth = TRUE,
        #   columnDefs = list(list(width = '10%', targets = c(1, 3)))
        # ),
        escape = FALSE,
        rownames= FALSE,
        selection = 'none'
        ) %>%
      formatStyle(
        columns=colnames(res[[1]]),
        style="bootstrap",
        backgroundColor = tablebgcolor(),
        color = tablecolor()
        )
      }
    )
  

  # UI rending of search results
  output$panel1 <<- if (searchSwitch %in% c("STOP", "chrbp37", "chr", "chrbpRange", "geneID")) {
    renderUI(tagList(
    h4("Gene Results:"
      # "Current build:",
      # ifelse(
      #   input$buildSwitch,
      #   "hg38",
      #   "hg19"
      #   )
      ),
    resultTable
  ))
  } else {
    renderUI(tagList(div()))
  }
  
  output$panel1b <<- if (searchSwitch %in% c("STOP", "chrbp37", "chrbpRange", "rsID")) {
    renderUI(tagList(
    h4("Variant Results:"),
    renderDT(
      {
        datatable(
          res[[2]],
          # options = list(
          #   autoWidth = TRUE,
          #   columnDefs = list(list(width = '10%', targets = c(1, 3)))
          # ),
          escape = FALSE,
          rownames= FALSE,
          selection = 'none'
        ) %>%
          formatStyle(
            columns=colnames(res[[2]]),
            style="bootstrap",
            backgroundColor = tablebgcolor(),
            color = tablecolor()
          )
      }
    )
  ))
  } else {
    renderUI(tagList(div()))
  }


}

# initiates search function on hitting the minisubmit button
observeEvent(
    input$minisearchBar_search,{
      runSearchPage()
      show(id = "searchResults")
      if (resultHidden) {
        startAnim(session,
                  id = "searchResults",
                  type = "slideInDown")
        resultHidden <<- F
        output$resultArrowUI <- renderUI({
          actionLink(
            "resultArrow",
            "",
            style = "display:inline-block;padding-top:30%",
            icon = 
              icon("chevron-up")
          )
        })
      }
      # Tutorial
      tutorial <<- "result"
      if (input$tutorial.mode == T) {
        output$tutorial <- tutorial.gene
      }
    }
  )

observeEvent(
  input$resultArrow,
  {
    if (resultHidden) {
      show(id = "searchResults")
      startAnim(session,
                id = "searchResults",
                type = "slideInDown")
      resultHidden <<- F
      output$resultArrowUI <- renderUI({
        actionLink(
          "resultArrow",
          "",
      
          icon = 
            icon("chevron-up")
        )
      })
    } else {
      startAnim(session,
                id = "searchResults",
                type = "slideOutUp")
      delay(900, {
        hide(id = "searchResults")
        removeClass(
          id = "searchResults",
          class = "animated slideOutUp"
        )
        })
      resultHidden <<- T
      output$resultArrowUI <- renderUI({
        actionLink(
          "resultArrow",
          "",
      
          icon = 
            icon("chevron-down")
        )
      })
    }
    
  }
)

#==DISABLED==refreshes search function on hitting the buildSwitch
# observeEvent(
#   input$buildSwitch,
#   runSearchPage()
# )

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
