#====MAIN function for search
# currently accepted input formats:
# 1. Gene ID (default)
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

######## TO DO- integrate searchFunction + searchFunctionVar to make it more efficient

# searchFunction <- function (searchGene, searchString = input$searchBar, type) {
#   if (type == "STOP") {
#     dat <- geneList[geneList$chr == "A"]
#   } else {
#   # this is setting up for base pair range search
#   dat <- geneList #if(searchGene) {geneList} else {varList}
#   preOverlap <- dat[grepl(gsub("^(\\d+):.*$", "\\1", searchString, ignore.case = T),
#         dat$chr,
#         ignore.case = T)]
#   if (grepl("\\d+:\\d+", searchString)) {
#     chrom <- gsub("^(\\d+):.*$", "\\1", searchString, ignore.case = T)
#   }
#     ranges <- data.table(`37bp1` = as.numeric(gsub("^\\d+:(\\d+)-\\d+$", "\\1", searchString, ignore.case = T)), `37bp2` = as.numeric(gsub("^\\d+:\\d+-(\\d+)$", "\\1", searchString, ignore.case = T)))
#     setkey(ranges, `37bp1`, `37bp2`)
#     #==DISABLED== buildSwitch hg19 or hg38
#   # if (input$buildSwitch == F) {
#   #   ranges <- data.table(`37bp1` = as.numeric(gsub("^\\d+:(\\d+)-\\d+$", "\\1", searchString, ignore.case = T)), `37bp2` = as.numeric(gsub("^\\d+:\\d+-(\\d+)$", "\\1", searchString, ignore.case = T)))
#   #   setkey(ranges, `37bp1`, `37bp2`)
#   # } else {
#   #   ranges <- data.table(`38bp1` = as.numeric(gsub("^\\d+:(\\d+)-\\d+$", "\\1", searchString, ignore.case = T)), `38bp2` = as.numeric(gsub("^\\d+:\\d+-(\\d+)$", "\\1", searchString, ignore.case = T)))
#   #   setkey(ranges, `38bp1`, `38bp2`)
#   # }
#   switch(type,
#          geneID = dat[grepl(searchString, dat$id, ignore.case = T)],
#          chr = dat[grepl(gsub("^chr(\\d+)", "\\1", searchString, ignore.case = T), dat$chr, ignore.case = T)],
#          chrbp37 = {
#            dat <- dat[grepl(chrom, dat$chr)] #first subset by chromosome number
#            bp <- as.numeric(gsub("^\\d+:(\\d+).*", "\\1", searchString, ignore.case = T))
#            dat[bp >= dat$`37bp1` & bp <= dat$`37bp2`] # then see if provided bp is in any bp range of the genes
#          },
#          chrbp38 = {
#            dat <- dat[grepl(chrom, dat$chr)] #first subset by chromosome number
#            bp <- as.numeric(gsub("^\\d+:(\\d+)$", "\\1", searchString, ignore.case = T))
#            dat[bp >= dat$`38bp1` & bp <= dat$`38bp2`] # then see if provided bp is in any bp range of the genes
#          },
#          # look for overlap between provided bp ranges
#          # add chromosome at some point?????
#          chrbpRange = foverlaps(preOverlap, ranges, nomatch = NULL)[,c(3:9)],
#          #rsID = dat[grepl(searchString, dat$rsID, ignore.case = T)]
#          rsID = dat[grepl("NA", dat$Chr)]
#   )
#   }
# }
# 
# searchFunctionVar <- function (searchString = input$searchBar, type) {
#   if (type %in% c("chr", "chrbp38", "STOP")) {
#     varList.nested[[1]][0]
#   } else if (type == "rsID"){
#     rsFirst <- gsub("^(rs\\d\\d)\\d+$", "\\1", searchString, ignore.case = F)
#     dat <- varList.rsID.nested[[rsFirst]]
#     dat[grepl(searchString, dat$rsID)]
#   } else if (grepl("^(\\d+):.*$", searchString, ignore.case = T)) {
#     chrom <- gsub("^(\\d+):.*$", "\\1", searchString, ignore.case = T)
#     dat <- varList.nested[[chrom]]
#     ranges <- data.table(`37bp1` = as.numeric(gsub("^\\d+:(\\d+)-\\d+$", "\\1", searchString, ignore.case = T)), `37bp2` = as.numeric(gsub("^\\d+:\\d+-(\\d+)$", "\\1", searchString, ignore.case = T)))
#     setkey(ranges, `37bp1`, `37bp2`)
#     #==DISABLED== buildSwitch hg19 or hg38
#     # if (input$buildSwitch == F) {
#     #   ranges <- data.table(`37bp1` = as.numeric(gsub("^\\d+:(\\d+)-\\d+$", "\\1", searchString, ignore.case = T)), `37bp2` = as.numeric(gsub("^\\d+:\\d+-(\\d+)$", "\\1", searchString, ignore.case = T)))
#     #   setkey(ranges, `37bp1`, `37bp2`)
#     # } else {
#     #   ranges <- data.table(`38bp1` = as.numeric(gsub("^\\d+:(\\d+)-\\d+$", "\\1", searchString, ignore.case = T)), `38bp2` = as.numeric(gsub("^\\d+:\\d+-(\\d+)$", "\\1", searchString, ignore.case = T)))
#     #   setkey(ranges, `38bp1`, `38bp2`)
#     # }
#     switch(type,
#            chrbp37 = {
#              bp <- as.numeric(gsub("^\\d+:(\\d+).*", "\\1", searchString, ignore.case = T))
#              dat[bp >= dat$`37bp1` & bp <= dat$`37bp2`]
#              },
#            # chrbp37 = subset( #first subset by chromosome number
#            #   dat,
#            #   # then see if provided bp is in any bp range of the genes
#            #   inrange(
#            #     as.numeric(gsub("^\\d+:(\\d+)$", "\\1", searchString, ignore.case = T)),
#            #     as.numeric(dat$`37bp1`)-1000,
#            #     as.numeric(dat$`37bp2`)+1000
#            #   )
#            # ),
#            chrbp38 = subset( #first subset by chromosome number
#              dat,
#              # then see if provided bp is in any bp range of the genes
#              inrange(
#                as.numeric(
#                  gsub("^\\d+:(\\d+)$",
#                       "\\1",
#                       searchString,
#                       ignore.case = T)
#                  ),
#                dat$`38bp1`,
#                dat$`38bp2`
#              )
#            ),
#            # look for overlap between provided bp ranges
#            # add chromosome at some point?????
#            chrbpRange = foverlaps(dat, ranges, nomatch = NULL)[,c(3:9)]
#     )
#   } else {
#     varList.nested[[1]][0]
#   }
# }

searchFunction <- function (searchGene, searchString = input$searchBar, type) {
  dat.nested <- list()
  if (type == "chrbpRange") {
    ranges <- data.table(`37bp1` = as.numeric(gsub("^\\d+:(\\d+)-\\d+$", "\\1", searchString, ignore.case = T)), `37bp2` = as.numeric(gsub("^\\d+:\\d+-(\\d+)$", "\\1", searchString, ignore.case = T)))
    setkey(ranges, `37bp1`, `37bp2`)
  }
  if (type %ni% c("rsID", "geneID", "STOP")) {
    chrom <- gsub("^(\\d+):.*$", "\\1", searchString, ignore.case = T)
    dat.nested[[1]] <- geneList[grepl(chrom, geneList$chr)]
    dat.nested[[2]] <- varList.nested[[chrom]]
  }
  switch(type,
         STOP = {
           # empty
           dat.nested[[1]] <- geneList[geneList$chr == "A"]
           # empty
           dat.nested[[2]] <- varList.nested[[1]][0]
         },
         rsID = {
           rsFirst <- gsub("^(rs\\d\\d)\\d+$", "\\1", searchString, ignore.case = F)
           dat.Var <- varList.rsID.nested[[rsFirst]]
           dat.nested[[2]] <- dat.Var[grepl(searchString, dat.Var$rsID)]
           # empty
           dat.nested[[1]] <- geneList[geneList$chr == "A"]
         },
         chr = {
           dat.nested[[1]] <- geneList[grepl(gsub("^chr(\\d+)", "\\1", searchString, ignore.case = T), geneList$chr, ignore.case = T)]
           # empty
           dat.nested[[2]] <- varList.nested[[1]][0]
         },
         chrbp37 = {
           bp <- as.numeric(gsub("^\\d+:(\\d+).*", "\\1", searchString, ignore.case = T))
           dat.nested[[1]] <- dat.nested[[1]][bp >= dat.nested[[1]]$`37bp1` & bp <= dat.nested[[1]]$`37bp2`] # then see if provided bp is in any bp range of the genes
           dat.nested[[2]] <- dat.nested[[2]][bp >= dat.nested[[2]]$`37bp1` & bp <= dat.nested[[2]]$`37bp2`]
         },
         chrbp38 = {
           bp <- as.numeric(gsub("^\\d+:(\\d+).*", "\\1", searchString, ignore.case = T))
           dat.nested[[1]] <- dat.nested[[1]][bp >= dat.nested[[1]]$`38bp1` & bp <= dat.nested[[1]]$`38bp2`] # then see if provided bp is in any bp range of the genes
           dat.nested[[2]] <- dat.nested[bp >= dat.nested$`38bp1` & bp <= dat.nested$`38bp2`]
         },
         chrbpRange = {
           dat.nested[[1]] <- foverlaps(dat.nested[[1]], ranges, nomatch = NULL)[,c(3:9)]
           dat.nested[[2]] <- foverlaps(dat.nested[[2]], ranges, nomatch = NULL)[,c(3:9)]
         },
         geneID = {
           dat.nested[[1]] <- geneList[grepl(searchString, geneList$id, ignore.case = T)]
           dat.nested[[2]] <- varList.nested[[1]][0]
         }
  )
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
  # if initial main page, then read from big search bar
  # else read from mini search bar
  searchSelect <- input$minisearchBar
  listSwitch <- !grepl("^rs\\d*", searchSelect, ignore.case = T)
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
    if (grepl("^rs\\d{1,3}$", searchSelect, ignore.case = T)) {
      # throw error if rsID is < 3 digits long
      sendSweetAlert(
        session,
        title = "Search field error!",
        text = 'Please provide at least four digits in the rsID.',
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
  colnames(res[[2]]) <- c("Position (Ref/Alt)", "Chromosome", "BP-Start", "BP-End", "Nearest Gene ID", "rsID")
  # for (i in 1:nrow(res[[2]])) {
  #   res[[2]]$`Position (Ref/Alt)`[i] <- paste0('<a id="', res[[2]]$`Position (Ref/Alt)`[i], '" href="#" onclick="varResClick(this.id)">', res[[2]]$`Position (Ref/Alt)`[i], '</a>')
  # }
  # prepping result page
  #   below segment specifically sets up the geneID elements to have
  #   a javascript function associated with it on click. On click, it
  #   should open the gene information page. "See clickdetect.js"
  #   for details.

  
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
  output$panel1 <<- renderUI(tagList(
    h4("Gene Results:"
      # "Current build:",
      # ifelse(
      #   input$buildSwitch,
      #   "hg38",
      #   "hg19"
      #   )
      ),
    resultTable
  )
  )
  
  output$panel1b <<- renderUI(tagList(
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


}


# initiates search function on hitting the minisubmit button
observeEvent(
    input$minisearchBar_search,
    runSearchPage()
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
