#====Dark Mode observeEvent====
observeEvent(input$darktheme, {
  if (input$darktheme == T) {
    output$darktheme <<- renderUI({
      tags$head(tags$link(rel = "stylesheet", type = "text/css",
                          href = "darktheme.css"#shinytheme("darkly")
      ))
    })
    
    output$mainlogo <- renderUI(tagList(img(src = "ipdgc_gb_dark.png", class = "mainlogo")))
    output$wrapperlogo <- renderUI(tagList(img(src = "ipdgc_gb_small_dark.png", id = "wrapperlogo")))

  } else {
    output$darktheme <<- renderUI({
      tags$head(tags$link(rel = "stylesheet", type = "text/css",
                                                       href = "theme.css" #shinytheme("yeti")
                          ))
    })
    output$mainlogo <- renderUI(tagList(img(src = "ipdgc_gb.png", class = "mainlogo")))
    output$wrapperlogo <- renderUI(tagList(img(src = "ipdgc_gb_small.png", id = "wrapperlogo")))

  }
}
)
#=============================

tablecolor <- reactive({
  if (input$darktheme == T) {
    'white'
  } else {
    "black"
  }
})

tablebgcolor <- reactive({
  if (input$darktheme == T) {
    'rgba(56, 56, 56)'
    #'#282828'
  } else {
    '#FFFFFF'
  }
})