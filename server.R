# Server.R is where most of the heavy lifting happens.

server <- function(input, output, session) {
  runfromPlotly <<- F
  #====initialization
  shinyjs::show("startLogo")
  startAnim(session,
            id = "startLogo",
            type = "flipInX")
  shinyjs::runjs(
    "Shiny.onInputChange('layout', false);"
  )
  delay(1500,
        {
          shinyjs::show("uiPage")
          shinyjs::show("fixedDLButton")
          shinyjs::hide("loadingPage")
          shinyjs::runjs(
            "dimension = document.getElementById('resultbox').offsetWidth;
            Shiny.onInputChange('dimension', dimension);"
          )
          delay(1000, {
            window.size <- reactive({
              ifelse(input$layout, input$dimension/2, input$dimension)
            })
            window.size.slow <- debounce({
              window.size
            },
            4000
            )
          })
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
                 if (input$layout) {
                   boxWidth <<- 6
                 } else {
                   boxWidth <<- 12
                 }
               })
  
  observeEvent(
    event_data("plotly_click"),
    {
      if (is.null(event_data("plotly_click"))) {
        NULL
      } else {
        eventdat <<- as.data.table(event_data("plotly_click")[3])
        print(eventdat[1,1])
        print(eventdat$x[1])
        runfromPlotly <<- T
        #plotlyID.integer <- gsub("^\\s*(\\d+).*$", "\\1", eventdat[1,1])
        #plotlyID <- gsub("^\\s*(\\d+).*$", "\\1", eventdat[1,1])
        #print(plotlyID)
        #print(plotlyID.integer)
        plotlyVariantTable <<- variantTable.global[grepl(eventdat$x[1], HG19_ID)]#[grepl(paste0("^.*", plotlyID, ".*$"), HG19_ID)]
        runjs("Shiny.setInputValue('varClick', Math.random());")
      }
    })
  
  # Update depending on search
  # e.g. add ?gene=LRRK2
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
  
  if (!exists("runFromURL")) {
    runFromURL <<- F
  }
}