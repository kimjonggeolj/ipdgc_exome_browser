# Server.R is where most of the heavy lifting happens.

server <- function(input, output, session) {
  # variable for remembering the selected result
  output$mainlogo <- renderUI(tagList(img(src = "ipdgc_gb.png", class = "mainlogo")))

  
  
  # ==== Sets up the "initial main page" (search page)
  initmainPage <- renderUI(tagList(fluidRow(uiOutput("mainlogo"),
                            mainSearchBar,
                            div(actionButton("submit", "Search"), style = "text-align:center;", id = "aboutClick")
                            )
                            ))
  
  source("darktheme.R", local = T)

  
  output$mainPage <- initmainPage

  # returns to search page when wrapperlogo image is clicked
  
  observeEvent(input$wrapperlogo, {
    output$mainPage <- initmainPage
    pageState <<- 1
    hide(id = "wrapperlogo")
    hide(id = "mainPageLink")
    hide(id = "miniSearchBar")
    hide(id = "minisubmit")
  })


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
  source("googlelogin-server.R", local = T)
  pageState <<- 1
}