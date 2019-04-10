library(shiny)
library(data.table)
library(shinyjs)
library(DT)

ui <- uiOutput("wrapUI")


server <- function(input, output) {
  rememberedPage <- list()
  initmainPage <- renderUI(tagList(fluidRow(img(src = "IDPGC-logo-01.png", class = "mainlogo"),
                            h2("Variant Browser", style = "text-align:center;"),
                            div(textInput("searchBar",
                                          label = NULL,
                                          placeholder = "e.g. LRRK2, rs201055865",
                                          width = "100%"), class = "searchbar"),
                            div(actionButton("submit", "Search"), style = "text-align:center;"))))
  output$mainPage <- initmainPage
  
  output$wrapUI <- renderUI(tagList(
    fluidPage(title = "IPDGC Variant Browser",
              useShinyjs(),
              theme = "theme.css",
              fluidRow(
                column(width = 6,
                       hidden(div(actionLink("main", "Main"), id = "mainPageLink"))),
                column(width = 6,
                       div(actionLink("about", "About"), id = "top-row")),
                hr()),
              uiOutput("mainPage")
    )
  ))
  
  source("datatables.R", local = T)
  
  #observeEvent for result page
  source("result-page.R", local = T)

  source("about-page.R", local = T)
}


shinyApp(ui = ui, server = server)

