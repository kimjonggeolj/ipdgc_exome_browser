# This is the "UI file" of the app.
miniSearchBar <- searchInput(
  inputId = "minisearchBar", 
  label = "Enter your search terms:", 
  placeholder = "e.g. LRRK2, chr12, 12:40196744, 12:40000000-50000000, rs1422910994", 
  btnSearch = icon("search"), 
  btnReset = icon("remove"), 
  width = "80%"
)

tagList(
  
  
  #========Below is part I of sample Google login API==========
  
  # tags$head(
  #   tags$meta(name="google-signin-scope",content="profile email"),
  #   tags$meta(name="google-signin-client_id", content="YOURCLIENTID.apps.googleusercontent.com"),
  #   HTML('<script src="https://apis.google.com/js/platform.js?onload=init"></script>'),
  #   includeScript("signin.js")
  # ),
  
  #============================================================
  
# Initialize Shinyjs + Shinyanimate
  useShinyjs(),
  withAnim(),
  tags$script(src = 'jquery-ui.min-draggable+position.js'),
  # tags$script(src = 'jquery-ui-draggable.js'),
  # tags$script(src = "jquery-ui.min.js"),
  # includeCSS("www/jquery-ui.min.css"),
# theme + javascript for detecting clicks for search results
  includeCSS("www/theme.css"),
  tags$script(src = "clickdetect.js"),
  tags$script(src = "boxsize.js"),
  # tags$script(src = "draggable.js"),
# dark theme
  uiOutput("darktheme"),
# Start logo
  div(
    id = "loadingPage",
    hidden(img(src = "ipdgc_eb.png", id = "startLogo"))
  ),
  hidden(
# fixed option button
    div(
      id = "fixedDLButton",
      dropdown(
# hg19 vs hg38 switch
        # prettyToggle(
        #   status_off = "primary",
        #   status_on = "danger",
        #   # icon_on = icon("cog"),
        #   # icon_off = icon("cog"),
        #   animation = "pulse",
        #   inputId = "buildSwitch",
        #   label_on = "hg38",
        #   label_off = "hg19",
        #   inline = T,
        #   bigger = T
        # ),
# dark theme switch
        prettyToggle(
          inputId = "darktheme",
          animation = "pulse",
          status_off = "primary",
          status_on = "warning",
          label_on = "Dark Theme",
          label_off = "Light Theme",
          inline = T,
          bigger = T
        ),
# layout switch (stacked vs pyramid)
        # switchInput(
        #   inputId = "layout",
        #   label = "Layout",
        #   onLabel = icon("boxes"),
        #   offLabel = icon("stream"),
        #   size = "default"
        # ),
        style = "jelly",
        # circle = T,
        status = "primary",
        icon = icon("cogs"),
        width = "150px",
        up = T,
        right = T
      )
    ),
# main UI
    div(
      id = "uiPage",
      dashboardPagePlus(
# header
        header = dashboardHeaderPlus(
          title = uiOutput("wrapperlogo"),
          left_menu = tagList(
            fluidRow(
              column(
                width = 7,
                miniSearchBar
              ),
              column(
                width = 4,
                h3(
                  ifelse(isDemo, "DEMO - LRRK2 ONLY", "ALPHA VERSION")
                  )
              ),
              column(
                width = 1,
                style = "text-align:right",
                div(
                  div(HTML("&nbsp;"),
                      style = "font-size:14px;"),
                  actionLink(
                    "about",
                    HTML("About&nbsp;&nbsp;")#,
                    #icon = icon("question")
                    ),
                  id = "top-row"
                )
              )
            )
          )
        ),
        sidebar = dashboardSidebar(
          disable = T
        ),
        body = dashboardBody(
          hidden(
            div(
              h3("Search Results", style = "display:inline-block;padding-left:15px;"),
              div(
                uiOutput("panel1"),
                style = 'padding-left:15px;padding-right:15px;'
              ),
              div(
                uiOutput("panel1b"),
                style = 'padding-left:15px;padding-right:15px;'
              ),
              #style = "width:100%;",
              id = 'searchResults'
            )
          ),

          div(
            hidden(
              div(id = "geneBoxes",
                  column(
                    width = 4,
                    boxPlus(
                      uiOutput("geneInfo"),
                      width = 12,
                      closable = F,
                      status = "warning"
                    ),
                    boxPlus(
                      uiOutput("geneNumbers"),
                      width = 12,
                      closable = F,
                      status = "danger"
                    ),
                    boxPlus(
                      uiOutput('geneLinks'),
                      width = 12,
                      closable = F,
                      status = "success"
                    )
                  ),
                  boxPlus(
                    uiOutput("geneWaffle"),
                    width = 5,
                    closable = F,
                    status = "success"
                  ),
                  boxPlus(
                    uiOutput("geneWaffleTable"),
                    width = 3,
                    closable = F,
                    status = "success"
                  ),
                  boxPlus(
                    uiOutput("geneNeedle"),
                    width = 12,
                    closable = F,
                    status = "info"
                  ),
                  boxPlus(
                    uiOutput("geneVartable"),
                    width = 12,
                    closable = F,
                    status = "danger"
                  )
                  )
            ),
            style = "padding-top: 15px;"
              ),
          uiOutput("varBox")
        ),
        title = "IPDGC Exome Browser",
        sidebar_fullCollapse = T,
        skin = "black"
      )
    )
  )
)

#========Below is part II of sample Google login API==========
# , div(id="signin", class="g-signin2", "data-onsuccess"="onSignIn"),
# actionButton("signout", "Sign Out", onclick="signOut();", class="btn-danger"),
# with(tags, dl(dt("Name"), dd(textOutput("g.name")),
#               dt("Email"), dd(textOutput("g.email")),
#               dt("Image"), dd(uiOutput("g.image")) ))
#=============================================================