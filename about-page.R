observeEvent(input$about, {
  showModal(modalDialog(
    size = "l",
    h1('IPDGC Exome Browser'),
    "IPDGC Exome Browser is a database of exome variants related to neurodegenerative diseases that can be used by researchers all over the world.",
    hr(),
    h4("Current database statistics"),
    renderTable(data.table(`Total Variant Events` = "INSERT NUM HERE", `Total Subjects` = "INSERT NUM HERE")),
    hr(),
    # h4("For more information on the data, please contact:"),
    # fluidRow(
    #   column(
    #     width = 6,
    # div(style = "margin-top:12px;",
    #     span("Nicholas Wood", style = "font-weight:bold; font-size:110%; margin-top:12px"),
    #     div(a(href="mailto:n.wood@ucl.ac.uk", "n.wood@ucl.ac.uk")),
    #     div("Department of Molecular Neuroscience"),
    #     div("Institute of Neurology"),
    #     div("University College London"),
    #     div("United Kingdom")
    # )
    #   ),
    #   column(
    #     width = 6,
    #     span("Cornelis Blauwendraat", style = "font-weight:bold; font-size:110%; margin-top:12px"),
    #     div(a(href="mailto:cornelis.blauwendraat@nih.gov", "cornelis.blauwendraat@nih.gov")),
    #     div("Molecular Genetics Section"),
    #     div("Laboratory of Neurogenetics"),
    #     div("National Institute on Aging"),
    #     div("National Institutes of Health"),
    #     div("Bethesda, Maryland 20892"),
    #     div("United States")
    #   )
    # ),
    # h4("For feedback/bug reports/questions regarding the app:"),
    # fluidRow(
    #   column(
    #     width = 6,
    #     div(#style = "margin-top:12px;",
    #       span("Jonggeol Jeffrey Kim", style = "font-weight:bold; font-size:110%; margin-top:12px"),
    #       div(a(href="mailto:kimjoj@nih.gov", "kimjoj@nih.gov")),
    #       div("Molecular Genetics Section"),
    #       div("Laboratory of Neurogenetics"),
    #       div("National Institute on Aging"),
    #       div("National Institutes of Health"),
    #       div("Bethesda, Maryland 20892"),
    #       div("United States")
    #     )
    #   ),
    #   column(
    #     width = 6,
    #     div(#style = "margin-top:12px;",
    #       span("Mary Makarious", style = "font-weight:bold; font-size:110%; margin-top:12px"),
    #       div(a(href="mailto:makariousmb@nih.gov", "makariousmb@nih.gov")),
    #       div("Statistical Genetics Section"),
    #       div("Laboratory of Neurogenetics"),
    #       div("National Institute on Aging"),
    #       div("National Institutes of Health"),
    #       div("Bethesda, Maryland 20892"),
    #       div("United States")
    #     )
    #   )
    # ),
    # h4("Debug data"),
    # p("pixelratio",session$clientData$pixelratio),
    # p("wrapperlogowidth", session$clientData$output_wrapperlogo_width),
    # p("url_search", session$clientData$url_search),
    # p("url_pathname", session$clientData$url_pathname),
    # p("session", str(reactiveValuesToList(input))),
    # p("window.size.slow", window.size.slow()),
    fluidRow(
      a(img(src = "ipdgc.png", style = "height:100px;"), href = "https://pdgenetics.org/", target = "_blank"),
      a(img(src = "nia-logo.png", style = "height:50px;"), href = "https://www.nia.nih.gov/", target = "_blank"))
      # column(
      #   width = 3,
      #   a(img(src = "ipdgc.png", style = "height:100px;"), href = "https://pdgenetics.org/", target = "_blank")
      #   ),
      # column(
      #   width = 3,
      #   a(img(src = "nia-logo.png", style = "height:50px;"), href = "https://www.nia.nih.gov/", target = "_blank"))
      # )
  ))
})