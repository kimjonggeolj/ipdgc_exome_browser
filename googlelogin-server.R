output$g.name = renderText({ input$g.name })
output$g.email = renderText({ input$g.email })
output$g.image = renderUI({ img(src=input$g.image) })