# Update depending on search
# e.g. add ?gene=LRRK2
observe({
  query <<- parseQueryString(session$clientData$url_search)
  if (!is.null(query[['gene']])) {
    runFromURL <<- T
    updateTextInput(session, "minisearchBar", value = query[['gene']])
    updateTextInput(session, "minisearchBar_text", value = query[['gene']])
    # runSearchPage()
    runjs("Shiny.setInputValue('geneClick', Math.random())")
  } else if (!is.null(query[['variant']])) {
    runFromURL <<- T
    updateTextInput(session, "minisearchBar", value = query[['variant']])
    updateTextInput(session, "minisearchBar_text", value = query[['variant']])
    # runSearchPage()
    print(paste("Query is", query[['variant']]))
    runjs("Shiny.setInputValue('varResClick', Math.random())")
    # searchString <- query[['variant']]
    # var <- createVarFunc(searchString)
    # varPageFunc(var)
  }
})

if (!exists("runFromURL")) {
  runFromURL <<- F
}