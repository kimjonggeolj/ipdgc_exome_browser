# Server.R is where most of the heavy lifting happens.

shinyServer(function(input, output, session) {
  tutorial <<- "search"
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
  # jquery for draggable window
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
  
  sendSweetAlert(
    session = session,
    title = "Terms of Use",
    text = tags$ol(
      tags$li(
        "By proceeding, you are agreeing to:",
        tags$ul(
          tags$li("Use the data for", a("health, medical, and biomedical research ONLY", href = "https://osp.od.nih.gov/wp-content/uploads/standard_data_use_limitations.pdf", target = "_blank")),
          tags$li("NOT Attempt to identify, disclose, or contact research participants unless required by federal, state, or local laws"),
          tags$li("Report any data management incidents including, but not limited to inadvertent data release"),
          tags$li("Abide by all relevant laws and regulations regarding genomic data and their use"),
          tags$li("Not bulk download data without explicit consent from the PD Variant Browser team")
        ),
        style = "text-align:left"
      ),
      tags$li(
        "While the data presented in the Variant Browser has undergone quality control, genomic data processing pipelines are inherently imperfect and rely on probabilistic processes such as variant calling, imputation, and short-read sequencing reads. As such there may be errors within the data presented. I agree that the PD Variant Browser team is not responsible for any incorrect data that may be present in the browser.",
        style = "text-align:left"
      )
    ),
      # div("By proceeding, you are agreeing to:",
      #          style = "text-align:left",
      #          tags$ul(
      #            tags$li("Use the data for", a("health, medical, and biomedical research ONLY", href = "https://osp.od.nih.gov/wp-content/uploads/standard_data_use_limitations.pdf", target = "_blank")),
      #            tags$li("NOT Attempt to identify, disclose, or contact research participants unless required by federal, state, or local laws"),
      #            tags$li("Report any data management incidents including, but not limited to inadvertent data release"),
      #            tags$li("Abide by all relevant laws and regulations regarding genomic data and their use"),
      #            tags$li("Not bulk download data without explicit consent from the PD Variant Browser team")
      #          ),
      #          hr(),
      #          "While the data presented in the Variant Browser has undergone quality control, genomic data processing pipelines are inherently imperfect and rely on probabilistic processes such as variant calling, imputation, and short-read sequencing reads. As such there may be errors within the data presented. I agree that the PD Variant Browser team is not responsible for any incorrect data that may be present in the browser."
      #          ),
    html = T,
    closeOnClickOutside = F,
    type = "info"
  )
  
}
)