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
  
  show("startLogo")
  
  startAnim(session,
            id = "startLogo",
            type = "fadeIn")
  
  delay(1500,
        {
          show("uiPage")
          show("fixedDLButton")
          hide("loadingPage")
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
  source("googlelogin-server.R", local = T)
  
  source('varinfo.Page.R', local = T)
}