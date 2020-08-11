# aboutText <- readLines("www/aboutText.txt")

aboutTable <- fread('www/aboutTable.tsv', sep = '\t')
# aboutTable$V1 <- gsub("Cases", "<p style='text-align: right;'>Cases</p>", aboutTable$V1)
# aboutTable$V1 <- gsub("Controls", "<p style='text-align: right;'>Controls</p>", aboutTable$V1)
# aboutTable$V1 <- gsub("Proxy-cases", "<p style='text-align: right;'>Proxy-cases</p>", aboutTable$V1)
# aboutTable$V1 <- gsub("All participants", "<strong>All participants</strong>", aboutTable$V1)

observeEvent(input$about, {
  showModal(modalDialog(
    size = "l",
    easyClose = T,
    h1("Parkinsons's Disease Variant Browser",socialButton(
      url = "https://github.com/kimjonggeolj/ipdgc_exome_browser",
      type = "github"
    )),
    p("Parkinson's Disease Variant Browser is a database of exome variants related to neurodegenerative diseases that can be used by researchers all over the world. As of April 2020, the browser contains over 6 million variants from summary-level UK Biobank exome data and various IPDGC whole genome sequencing, exome sequencing, and imputed genotyping array data."),
    p(
      "All data is in hg19. All data in hg38 was converted to hg19 using ",
      a("CrossMap", href = "http://crossmap.sourceforge.net/", target = "_blank"),
      ". Data was cleaned and merged using Python and ",
      a("PLINK 1.9/2.0 alpha", href = "https://www.cog-genomics.org/plink/", target = "_blank"),
      ". Variants were annotated with ",
      a("ANNOVAR", href = "http://annovar.openbioinformatics.org/", target = "_blank"),
      ". Detailed pipeline can be found in the manuscript or here: [coming soon]."),
    #aboutText,
    hr(),
    h4("Current database statistics"),
    # strong("Variants"),
    # renderTable(data.table(
    #   `Total Variant Events` = "6,126,687"
    #                        )),
    # strong("Datasets"),
    # data.table(
    #   `Total participants` = "104,930",
    #   `LNG Genomes` = "9,300",
    #   `IPDGC Exome Sequencing Project` = "5,088",
    #   `IPDGC Resequencing Project` = "5,209",
    #   `IPDGC array datasets (imputed)` = "45,306",
    #   `UKBiobank` = "40,027"
    # )
    renderTable(
      # colnames = F,
      # align = 'r',
      sanitize.text.function = function(x) x,
      aboutTable
      ),
    hr(),
    h4("For more information on the data, please contact:"),
    fluidRow(
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
      column(
        width = 4,
        span("Cornelis Blauwendraat", style = "font-weight:bold; font-size:110%; margin-top:12px"),
        div("cornelis.blauwendraat(@)nih.gov")#,
        # div("Molecular Genetics Section"),
        # div("Laboratory of Neurogenetics"),
        # div("National Institute on Aging"),
        # div("National Institutes of Health"),
        # div("Bethesda, Maryland 20892"),
        # div("United States")
      ),
      column(
        width = 4,
        span("Andrew Singleton", style = "font-weight:bold; font-size:110%; margin-top:12px"),
        div("singleta(@)mail.nih.gov")#,
        # div("Molecular Genetics Section"),
        # div("Laboratory of Neurogenetics"),
        # div("National Institute on Aging"),
        # div("National Institutes of Health"),
        # div("Bethesda, Maryland 20892"),
        # div("United States")
      ),
      column(
        width = 4,
        span("Mike Nalls", style = "font-weight:bold; font-size:110%; margin-top:12px"),
        div("mike(@)datatecnica.com")#,
        # div("Statistcal Genetics Section"),
        # div("Data Tecnica International"),
        # div("Laboratory of Neurogenetics"),
        # div("National Institute on Aging"),
        # div("National Institutes of Health"),
        # div("Bethesda, Maryland 20892"),
        # div("United States")
      ),
    ),
    h4("For feedback/bug reports/questions regarding the app:"),
    fluidRow(
      column(
        width = 6,
        div(#style = "margin-top:12px;",
          span("Jonggeol Jeffrey Kim", style = "font-weight:bold; font-size:110%; margin-top:12px"),
          div("kimjoj(@)nih.gov")#,
          # div("Molecular Genetics Section"),
          # div("Laboratory of Neurogenetics"),
          # div("National Institute on Aging"),
          # div("National Institutes of Health"),
          # div("Bethesda, Maryland 20892"),
          # div("United States")
        )
      ),
      column(
        width = 6,
        div(#style = "margin-top:12px;",
          span("Mary Makarious", style = "font-weight:bold; font-size:110%; margin-top:12px"),
          div("makariousmb(@)nih.gov")#,
          # div("Statistical Genetics Section"),
          # div("Laboratory of Neurogenetics"),
          # div("National Institute on Aging"),
          # div("National Institutes of Health"),
          # div("Bethesda, Maryland 20892"),
          # div("United States")
        )
      )
    ),
    hr(),
    h4("License/Permissions/Acknowledgements"),
    fluidRow(
      column(
        width = 12,
        p(a("This Shiny application is under GNU General Public License 3.", href = "https://github.com/kimjonggeolj/ipdgc_exome_browser/blob/master/LICENSE", target = "_blank")),
        p("All genetic data available are aggregate summary-level data and free to use for research purposes ONLY."),
        p("We thank our IPDGC collaborators, UK Biobank, and their participants for making their data available for use in the research community.")
      )
    ),
    # h4("Debug data"),
    # p("pixelratio",session$clientData$pixelratio),
    # p("wrapperlogowidth", session$clientData$output_wrapperlogo_width),
    # p("url_search", session$clientData$url_search),
    # p("url_pathname", session$clientData$url_pathname),
    # p("session", str(reactiveValuesToList(input))),
    # p("window.size.slow", window.size.slow()),
    fluidRow(
      uiOutput("modallogo")
      )
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