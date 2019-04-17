# Server.R is where most of the heavy lifting happens.

server <- function(input, output, session) {
  # variable for remembering the selected result
  storedRes <- list()
  #rememberedPage <- list()
  
  # ==== Sets up the "initial main page" (search page)
  initmainPage <- renderUI(tagList(fluidRow(img(src = "ipdgc_gb.png", class = "mainlogo"),
                            mainSearchBar,
                            div(actionButton("submit", "Search"), style = "text-align:center;", id = "aboutClick")
                            )#,
                            ))
  
  output$mainPage <- initmainPage
  # output$wrapUI <- renderUI(tagList(
  #   fluidPage(title = "IPDGC Variant Browser",
  #             useShinyjs(),
  #             theme = "theme.css",
  #             fluidRow(
  #               column(width = 6,
  #                      hidden(div(#actionLink("main", "Main"),
  #                                 actionLink("returnResults", "Return to results"), id = "mainPageLink"))),
  #               column(width = 6,
  #                      div(actionLink("about", "About"), id = "top-row")),
  #               hr()),
  #             uiOutput("mainPage")
  #   )
  # ))
  
  # returns to search page when wrapperlogo image is clicked
  
  observeEvent(input$wrapperlogo, {
    output$mainPage <- initmainPage
    pageState <<- 1
    hide(id = "wrapperlogo")
    hide(id = "mainPageLink")
    hide(id = "miniSearchBar")
    hide(id = "minisubmit")
  })
  
  observeEvent(input$darktheme, {
                 if (input$darktheme == T) {
                 output$darktheme <<- renderUI({
                   tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                                       href = shinytheme("cyborg")))
                 })
               } else {
                 output$darktheme <<- renderUI({tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                                                                    href = shinytheme("simplex")#"shiny/inst/www/shared/bootstrap/css/bootstrap-theme.min.css"
                                                                    ))})
               }
               }
  )
  
  #====initial data loading file
  source("datatables.R", local = T)
  
  #====search results
  source("result-page.R", local = T)
  
  #====result page for gene information
  source("geneinfo-page.R", local = T)

  #====about page
  source("about-page.R", local = T)
  
  #====Google sample of sign in API server side
  source("googlelogin-server.R", local = T)

}