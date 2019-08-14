# This is the "UI file" of the app.
ui <- tagList(
  
  
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
# theme + javascript for detecting clicks for search results
  includeCSS("www/theme.css"),
  tags$script(src = "clickdetect.js"),
  tags$script(src = "boxsize.js"),
# dark theme
  uiOutput("darktheme"),
# Start logo
  div(
    id = "loadingPage",
    hidden(img(src = "ipdgc_vb.png", id = "startLogo"))
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
          # icon_on = icon("moon"),
          # icon_off = icon("lightbulb"),
          animation = "pulse",
          status_off = "primary",
          status_on = "warning",
          label_on = "Dark Theme",
          label_off = "Light Theme",
          inline = T,
          bigger = T
        ),
# layout switch (stacked vs pyramid)
        switchInput(
          inputId = "layout",
          label = "Layout",
          onLabel = icon("boxes"),
          offLabel = icon("stream"),
          size = "default"
        ),
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
                width = 6,
                miniSearchBar
              ),
              column(
                width = 2,
                h2("ALPHA VERSION")
              ),
              column(
                width = 4,
                style = "text-align:right",
                div(
                  div(HTML("&nbsp;"),
                      style = "font-size:14px;"),
                  actionLink("about", HTML("&nbsp;"), icon =icon("question")),
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
          boxPlus(
            id = "resultbox",
            title = "Search Results",
            uiOutput("panel1"),
            uiOutput("panel1b"),
            width = 12,
            collapsible = T,
            closable = F
          ),
          uiOutput("geneBox"),
          uiOutput("varBox")
        ),
        title = "IPDGC Genome Browser",
        sidebar_fullCollapse = T,
        skin = "black"
      )
    )
  )
)



# This uiOutput allows rendering of UI inside this initial wrapper UI
# When you want to render a UI inside the wrapper, define a "mainpage" variable
# with renderUI() + whatever UI elements you want.
#uiOutput("mainPage"),



#========Below is part II of sample Google login API==========
# , div(id="signin", class="g-signin2", "data-onsuccess"="onSignIn"),
# actionButton("signout", "Sign Out", onclick="signOut();", class="btn-danger"),
# with(tags, dl(dt("Name"), dd(textOutput("g.name")),
#               dt("Email"), dd(textOutput("g.email")),
#               dt("Image"), dd(uiOutput("g.image")) ))
#=============================================================

#uiOutput("wrapUI")