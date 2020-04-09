# Server.R is where most of the heavy lifting happens.

shinyServer(function(input, output, session) {
  # Colorscale for needlePlot + waffle plot
  # colorList <- list()
  # colorList[[1]] <- c("synonymous SNV" = "#beaed4", "nonsynonymous SNV" = "#386cb0", "nonframeshift mutation" = "#7fc97f", "frameshift mutation" = "#fdc086", "stopgain" = "#ffff99", "stoploss" = "#f0027f",  "NA/unknown" = "#e8e6e4")
  # colorList[[2]] <- c("synonymous SNV" = "#beaed4", "nonsynonymous SNV" = "#386cb0", "nonframeshift" = "#7fc97f", "nonframeshift insertion" = "#7fc9c9", "nonframeshift deletion" = "#a4c97f", "nonframeshift block substitution" = "#5e915d",  "frameshift insertion" = "#fdc086", "frameshift deletion" = "#fddd86", "frameshift block substitution" = "#fda286", "stopgain" = "#ffff99", "stoploss" = "#f0027f", "NA/unknown" = "#e8e6e4")
  
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
            "dimension = document.getElementById('geneNeedle').offsetWidth;
      Shiny.onInputChange('dimension', dimension);"
          )
          delay(1000, {
            window.size <- reactive({
              input$dimension
            })
            window.size.slow <- debounce({
              window.size
            },
            4000
            )
          })
        }
  )
  
  # geneBoxHidden <<- T
  resultHidden <<- T
  
  #====Dark theme/light theme events and reactives
  source("darktheme.R", local = T)
  
  #====Tutorial mode
  source('tutorial.R', local = T)
  
  #====search results
  source("result-page.R", local = T)
  
  #====page for gene information
  source("geneinfo-page.R", local = T)

  #====about page
  source("about-page.R", local = T)
  
  #====Google sample of sign in API server side
  # source("googlelogin-server.R", local = T)
  
  source('varinfo.Page.R', local = T)
  
  # observeEvent(input$layout,
  #              {
  #                if (input$layout) {
  #                  boxWidth <<- 6
  #                } else {
  #                  boxWidth <<- 12
  #                }
  #              })
  # output$globalVarTable_DL <- downloadHandler(
  #   filename = "varTable.tsv",
  #   content = function(file) {
  #     fwrite(variantTable.global, file, sep = "\t")
  #   }
  # )
  observeEvent(
    event_data("plotly_click"),
    {
      if (is.null(event_data("plotly_click"))) {
        NULL
      } else {
        #eventdatDebug <<- event_data("plotly_click")
        eventdat <<- try(as.data.table(event_data("plotly_click")[3])) #this will error out if donut plot is clicked
        if (class(eventdat) != "try-error") { # if not donut plot
          #print(eventdat[1,1])
          #print(eventdat$x[1])
          runfromPlotly <<- T
          #plotlyID.integer <- gsub("^\\s*(\\d+).*$", "\\1", eventdat[1,1])
          #plotlyID <- gsub("^\\s*(\\d+).*$", "\\1", eventdat[1,1])
          #print(plotlyID)
          #print(plotlyID.integer)
          plotlyVariantTable <<- variantTable.global[grepl(eventdat$x[1], HG19_ID)]#[grepl(paste0("^.*", plotlyID, ".*$"), HG19_ID)]
          #plotlyVariantTable <<- variantTable.global[eventdat$x[1],]
          runjs("Shiny.setInputValue('varClick', Math.random());")
        }
      }
    })
  
  source('url.search.R', local = T)
  
  # Javascript that allows search by hitting enter
  runjs(
    'var searchBar = document.getElementById("minisearchBar_text");
    
    searchBar.addEventListener("keyup", function(event) {
  // Number 13 is the "Enter" key on the keyboard
  if (event.keyCode === 13) {
    // Cancel the default action, if needed
    event.preventDefault();
    // Trigger the button element with a click
    document.getElementById("minisearchBar_search").click();
  }
});'
  )
  delay(2000,
        runjs(
    'containmentTop = $(".content-wrapper").position().top;
  //mydiv = document.getElementById("draggable-top");
//var moveHeight = ((parseInt(document.body.clientHeight) / 2) - (parseInt(mydiv.offsetHeight) / 2)) +"px";
//var moveWidth = ((parseInt(document.body.clientWidth) / 2) - (parseInt(mydiv.offsetWidth) / 2)) +"px";
//NOTE THAT CONTAINMENT DOES NOT WORK ON START UP. May need to force javascript to re-run after content-wrapper loads.
  $(".draggable").draggable({ 
		cursor: "move",
		handle: ".box-header",
        cancel: ".box-body",
        containment: [,containmentTop,,]
  });'
  )
  )
}
)