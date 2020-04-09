#====Dark Mode observeEvent====
observeEvent(input$darktheme, {
  # === Dark mode ===
  if (input$darktheme == T) {
    output$darktheme <<- renderUI({
      tags$head(tags$link(rel = "stylesheet", type = "text/css",
                          href = "darktheme.css"#shinytheme("darkly")
      ))
    })
    
    #output$mainlogo <- renderUI(tagList(img(src = "ipdgc_gb_dark.png", class = "mainlogo")))
    output$wrapperlogo <- renderUI(
      tagList(
        img(src = "ipdgc_eb_small2_dark.png", id = "wrapperlogo", class = "logo-expanded"),
        img(src = "ipdgc_eb_mini_dark.png", id = "minilogo")
        )
      )
    
    output$modallogo <- renderUI(tagList(a(img(src = "ipdgcDark.png", style = "height:100px;"), href = "https://pdgenetics.org/", target = "_blank")))
    
  # === Light mode ===
    
  } else {
    output$darktheme <<- renderUI({
      tags$head(tags$link(rel = "stylesheet", type = "text/css",
                                                       href = "lighttheme.css" #shinytheme("yeti")
                          ))
    })
    #output$mainlogo <- renderUI(tagList(img(src = "ipdgc_gb.png", class = "mainlogo")))
    output$wrapperlogo <- renderUI(
      tagList(
        img(src = "ipdgc_eb_small2.png", id = "wrapperlogo", class = "logo-expanded"),
        img(src = "ipdgc_eb_mini.png", id = "minilogo")
        )
      )
    
    output$modallogo <- renderUI(tagList(a(img(src = "ipdgc.png", style = "height:100px;"), href = "https://pdgenetics.org/", target = "_blank")))
  }
})
#=============================

#===Dark Mode reactives for data tables===
tablecolor <- reactive({
  if (input$darktheme == T) {
    'white'
  } else {
    "black"
  }
})

tablebgcolor <- reactive({
  if (input$darktheme == T) {
    '#383838'
  } else {
    '#FFFFFF'
  }
})