# Server.R is where most of the heavy lifting happens.

server <- function(input, output, session) {
  # if (pageState == 3) {
  #   setBookmarkExclude(c("about", "darktheme", "minisubmit", "submit", "wrapperlogo", "returnResults"))
  # } else {
  #   setBookmarkExclude(c("about", "minisearchBar", "searchBar", "darktheme", "minisubmit", "submit", "wrapperlogo", "returnResults", "resPageId"))
  # }

  
  # observe({
  #   # Trigger this observer every time an input changes
  #   reactiveValuesToList(input)
  #   session$doBookmark()
  # })
  # onBookmarked(function(url) {
  #   updateQueryString(url)
  # })
  
  shinyjs::show("startLogo")

  
  startAnim(session,
            id = "startLogo",
            type = "flipInX")
  
  delay(1500,
        {
          shinyjs::show("uiPage")
          shinyjs::show("fixedDLButton")
          shinyjs::hide("loadingPage")
        }
  )
  
  source("darktheme.R", local = T)



  #====search results
  source("result-page.R", local = T)
  
  #====page for gene information
  source("geneinfo-page.R", local = T)
  
  #====page for variant information
  # observeEvent(input$varClick, {
  #   var <- varList[varList$id == toupper(input$varPageId)]
  # })

  #====about page
  source("about-page.R", local = T)
  
  #====Google sample of sign in API server side
  # source("googlelogin-server.R", local = T)
  
  source('varinfo.Page.R', local = T)
  
  observeEvent(input$layout,
               {
                 if (input$layout == T) {
                   boxWidth <<- 6
                 } else {
                   boxWidth <<- 12
                 }
               })
  
  # Update depending on search
  observe({
    query <<- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['gene']])) {
      runFromURL <<- T
      updateTextInput(session, "minisearchBar", value = query[['gene']])
      updateTextInput(session, "minisearchBar_text", value = query[['gene']])
      runSearchPage()
      runjs("Shiny.setInputValue('geneClick', Math.random())")
    } else if (!is.null(query[['variant']])) {
      runFromURL <<- T
      updateTextInput(session, "minisearchBar", value = query[['variant']])
      updateTextInput(session, "minisearchBar_text", value = query[['variant']])
      runSearchPage()
      runjs("Shiny.setInputValue('varResClick', Math.random())")
    }
    })
}