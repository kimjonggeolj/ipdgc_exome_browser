tutorial.search <- renderUI(
  tagList(
    h1("Welcome to the Parkinsons's Disease Variant Browser!"),
    p("You have turned on the tutorial mode.",
      "This mode will teach you the basics of the app while in use.",
      #tags$strong("As you use the app, you can click on \"What is this?\" to learn more in detail about a specific feature of the app."),
      "To deactivate tutorial mode, you can click the tutorial button (\"?\") to return to normal operations."),
    p(tags$strong("You can begin by using the search bar on the top of the browser."), "You can search by gene ID, rsID, chromosome number, variant location, or base pair range."),
    actionLink("tutorial.search.detail", label = h3("Can you go into more detail about searching?")),
    conditionalPanel(
      condition = 'input["tutorial.search.detail"]%2 == 1',
      p("To sear h for gene ID and rsID, just type them in.", tags$code("rs34637584"), "However, chromosome and base pair positions require specific syntax."),
      p("To search for", tags$strong("all genes in a specific chromosome,"), "write `chr` in front of the chromosome number (e.g.", tags$code("chr1"), ")."),
      p("To search for a", tags$strong("specific chromosome and base-pair position,"),"please write it in this format: [chr#]:[bp#]. For example, chromosome 1 base-pair position 155205634 should be entered as", tags$code("1:155205634"), "."),
      p("To search for a", tags$strong("chromosome and base-pair range,"),"please write it in this format: [chr#]:[bp1#]-[bp2#]. For example, chromosome 1 base-pair range 155205000 to 155210000 should be entered as", tags$code("1:155205000-155210000"), ".")
    )
  )
)
tutorial.result <- renderUI(
  tagList(
    h1('Tutorial-Search Results'),
    p("You have completed a search! Click on any variant or gene ID to continue."),
    actionLink("tutorial.result.empty", label = h3("My search results are empty!")),
    conditionalPanel(
      condition = 'input["tutorial.result.empty"]%2 == 1',
      p("If your results are empty, what you are looking for is not found in our data or there is an error in your search string. Please keep in mind that we only have exonic variants covered in", a("exome calling regions for gnomAD 2.0 (download link).", href = "https://storage.googleapis.com/gnomad-public/intervals/exome_calling_regions.v1.interval_list", target = "_blank" )),
      h4("For your convenience, the search string syntax is below:"),
      p("Searching for gene ID and rsID is straight forward. However, chromosome and base pair positions require specific syntax."),
      p("To search for all genes in a specific chromosome, write `chr` in front of the chromosome number (e.g.", tags$code("chr1"), ")."),
      p("To search for a", tags$strong("specific chromosome and base-pair position,"),"please write it in this format: [chr#]:[bp#]. For example, chromosome 1 base-pair position 155205634 should be entered as", tags$code("1:155205634"), "."),
      p("To search for a", tags$strong("chromosome and base-pair range,"),"please write it in this format: [chr#]:[bp1#]-[bp2#]. For example, chromosome 1 base-pair range 155205000 to 155210000 should be entered as", tags$code("1:155205000-155210000"), ".")
    ),
    p("To deactivate tutorial mode, you can click the tutorial button (\"?\") to return to normal operations.")
  )
)
tutorial.gene <- renderUI(
  tagList(
    h1('Tutorial-Gene Information'),
    p("This page will provide variant information on the specific gene you have chosen."),
    actionLink("tutorial.gene.plot", label = h3("Please explain the different plots.")),
    conditionalPanel(
      condition = 'input["tutorial.gene.plot"]%2 == 1',
      p("The sundial plot on the far right is the distribution of exonic variants associated with this gene categorized by their functional consequences. Blue represents nonframeshift variants, green represents frameshift variants, red represents stopgains, and violet represents stoplosses.", tags$strong("You can see detailed distribution of each categories by clicking on a category.")),
      p("The needle plot further down shows the distribution of the variants in their base-pair position.")
    ),
    actionLink("tutorial.gene.table", label = h3("Please explain the different tables.")),
    conditionalPanel(
      condition = 'input["tutorial.gene.table"]%2 == 1',
      p("Similarly to the sundial plot, first table displays the number of exonic variants in this gene categorized by their functional consequences. Blue represents nonframeshift variants, green represents frameshift variants, red represents stopgains, and violet represents stoplosses."),
      p("The variant table found on the bottom of the page lists the variants associated with this gene. The Exome name is in the following format:", tags$code("chr:bp:A2:A1"), ", where A1 is effectively the \"minor/effect allele\".")
    ),
    actionLink("tutorial.gene.download", label = h3("Can I download the plots and the tables?")),
    conditionalPanel(
      condition = 'input["tutorial.gene.download"]%2 == 1',
      p("Yes! To download the plots, hover over them and you will be able to find a camera button. It will save the plot as a PNG file."),
      p("To download the variant table, you can use one of the five buttons above the table to output the table in your preferred forrmat.")
    ),
    p("To deactivate tutorial mode, you can click the tutorial button (\"?\") to return to normal operations.")
  )
)
tutorial.variant <- renderUI(
  tagList(
    h1('Tutorial-Variant Information'),
    p("This window provides detailed information on a specific variant. At the top, you can see the general information on the variant, such as the variant name", tags$code("(CHR:BP:A2:A1)"), "and rsID. The hyperlinks will take you to different resources regarding the same variant."),
    p("Center of the window presents additional functional information and, if applicable, ClinVar annotations."),
    p("The bottom of the window provides frequency information found in this resource as well as those found in other resources such as gnomAD."),
    actionLink("tutorial.variant.distribution", label = h3("Please explain the how to read the \"distribution\" section on the variant frequency table.")),
    conditionalPanel(
      condition = 'input["tutorial.variant.distribution"]%2 == 1',
      p("The distribution row shows the number of participants with homozygous or heterozygous alleles. The counts are ordered:", tags$strong("Homozygous A1/Heterozyoug/Homozygous A2"))
      ),
    actionLink("tutorial.gene.proxy", label = h3("What is \"Proxy\"?")),
    conditionalPanel(
      condition = 'input["tutorial.gene.proxy"]%2 == 1',
      p("Proxy cases are individuals in UK Biobank who do not have Parkinson's Disease but have a first degree relative that does. While not clinical cases, they can be effectively seen as \"half\" a case, as they share half of their genome with a clinical case.")
    ),
    # actionLink("tutorial.gene.download", label = h3("Can I download the plots and the tables?")),
    # conditionalPanel(
    #   condition = 'input["tutorial.gene.download"]%2 == 1',
    #   p("Yes! To download the plots, hover over them and you will be able to find a camera button. It will save the plot as a PNG file."),
    #   p("To download the variant table, you can use one of the five buttons above the table to output the table in your preferred forrmat.")
    # ),
    p("To deactivate tutorial mode, you can click the tutorial button (\"?\") to return to normal operations.")
  )
)

observeEvent(input$tutorial.mode, {
  if (input$tutorial.mode == T) {
    show(
      id = "tutorial_bar"
    )
    show(
      id = "draggable-top-tutorial"
    )
    runjs(
      '$(".draggable-tutorial").position({
    my: "center center+25%",
    at: "center",
    of: window,
    using: function (pos, ext) {
        $(this).animate({ top: pos.top }, 400);
    }
})'
    )
    # showModal(modalDialog(
    #   size = "l",
    #   h2('Welcome to the IPDGC Exome Browser!'),
    #   p("You have turned on the tutorial mode of the browser. To deactivate, you can click the tutorial button (\"?\") to return to normal operations."),
    #   h4("The IPDGC Exome Browser is an open resource of summmary-level genomic data, with exome variants related to neurodegenerative diseases that can be used by researchers all over the world. In this tutorial, we will explain how to use this database to the fullest."),
    #   h4("At the top, you can find the searchbar. It can be used to search gene-level information or specific SNPs. First, we will show how you can search for specific variants. Please type in \"rs671\" in the searchbar then press Enter.")
    # ))
    if (tutorial == "search") {
      output$tutorial <- tutorial.search
    } else if (tutorial == "result") {
      output$tutorial <- tutorial.result
    } else if (tutorial == "gene") {
      output$tutorial <- tutorial.gene
    }
  } else if (input$tutorial.mode == F) {
    showNotification(
      "Deactivating tutorial mode",
      type = "message"
    )
    hide(
      id = "tutorial_bar"
    )
    hide(
      id = "draggable-top-tutorial"
    )
  }
  
    # column(
    #   width = 3,
    #   a(img(src = "ipdgc.png", style = "height:100px;"), href = "https://pdgenetics.org/", target = "_blank")
    #   ),
    # column(
    #   width = 3,
    #   a(img(src = "nia-logo.png", style = "height:50px;"), href = "https://www.nia.nih.gov/", target = "_blank"))
    # )
  
},
ignoreInit = T
)


# output$tutorial <- renderUI(tagList(
#   h4("At the top, you can find the searchbar. It can be used to search gene-level information or specific SNPs. First, we will show how you can search for specific variants. Please type in \"rs671\" in the searchbar then press Enter.")
# ))