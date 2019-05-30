observeEvent(input$varClick, {
  output$mainPage <- renderUI(tagList(
    fluidRow(
      column(width = 12,
             h1(tags$b("Variant:"), "chr12:40225170-40225170 (G/A)"),
             h2("rs775407458",
                tags$i("(LRRK2)"),
                tags$sup(
                  a(href = "https://www.ai-omni.com/search=rs775407458/page=1",
                    "omni"),
                  a(href = "https://gnomad.broadinstitute.org/variant/12-40618972-G-A",
                    "gnomad"),
                  a(href = "https://bravo.sph.umich.edu/freeze5/hg38/variant/12-40225170-G-A",
                    "bravo"),
                  a(href = "https://www.ncbi.nlm.nih.gov/snp/rs775407458",
                    "dbSNP")
                )),
             div(tags$b("Region:"), paste0("Exonic"), style = "margin-bottom:20px;"),
             div(tags$b("Amino Acid Change:"), paste0("p.E13E"), style = "margin-bottom:2px;"),
             div(tags$b("Functional Consequence:"), paste0("Synonymous SNV"), style = "margin-bottom:20px;"),
             div(tags$b("ClinVar:"), paste0("None"), style = "margin-bottom:50px;"),
             h3("Frequency Table")
      )
    ),
    fluidRow(column(width = 12,
           fluidRow(div(tableOutput("mytable"))))
    )
  )
  )
})



output$mytable = renderTable({
    mtcars[1:4,]
  })
