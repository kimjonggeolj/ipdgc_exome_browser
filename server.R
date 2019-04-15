# Server.R is where most of the heavy lifting happens.

server <- function(input, output, session) {
  # variable for remembering the selected result
  storedRes <- list()
  #rememberedPage <- list()
  
  # ==== Sets up the "initial main page" (search page)
  initmainPage <- renderUI(tagList(fluidRow(img(src = "ipdgc_vb.png", class = "mainlogo"),
                            div(textInput("searchBar",
                                          label = NULL,
                                          placeholder = "e.g. LRRK2, chr12, 12:40000000-50000000",
                                          width = "100%"), class = "searchbar"),
                            div(actionButton("submit", "Search"), style = "text-align:center;", onclick = '"aboutClick"'))#,
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
    hide(id = "wrapperlogo")
    hide(id = "mainPageLink")
  })
  
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