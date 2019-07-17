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
  
  # output$optionsButton <- renderUI(
  #   {
  #     div(
  #       id = "fixedDLButton",
  #       dropdown(
  #         prettyToggle(
  #           status_off = "primary",
  #           status_on = "danger",
  #           # icon_on = icon("cog"),
  #           # icon_off = icon("cog"),
  #           animation = "pulse",
  #           inputId = "buildSwitch",
  #           label_on = "hg38",
  #           label_off = "hg19",
  #           inline = T,
  #           bigger = T
  #         ),
  #         prettyToggle(
  #           inputId = "darktheme",
  #           # icon_on = icon("moon"),
  #           # icon_off = icon("lightbulb"),
  #           animation = "pulse",
  #           status_off = "primary",
  #           status_on = "warning",
  #           label_on = "Dark Theme",
  #           label_off = "Light Theme",
  #           inline = T,
  #           bigger = T
  #         ),
  #         style = "material-circle",
  #         # circle = T,
  #         status = ifelse(input$darktheme, "warning", "primary"),
  #         icon = icon("cogs"),
  #         width = "150px",
  #         up = T,
  #         right = T
  #         # tooltip = tooltipOptions(
  #         #   placement = "top",
  #         #   title = "Click me for options/settings"
  #         #   )
  #         # animate = animateOptions(
  #         #   enter = animations$fading_entrances$fadeInLeftBig,
  #         #   exit = animations$fading_exits$fadeOutRightBig
  #         # )
  #       )
  #       )
  #   }
  # )
  
  startAnim(session,
            id = "startLogo",
            type = "flipInX")
  
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