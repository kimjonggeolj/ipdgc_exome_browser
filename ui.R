# This is the "UI file" of the app.
# In reality, it only establishes the initial wrapper UI and a window for search results.
ui <- tagList(
  
  #========Below is part I of sample Google login API==========
  
  # tags$head(
  #   tags$meta(name="google-signin-scope",content="profile email"),
  #   tags$meta(name="google-signin-client_id", content="YOURCLIENTID.apps.googleusercontent.com"),
  #   HTML('<script src="https://apis.google.com/js/platform.js?onload=init"></script>'),
  #   includeScript("signin.js")
  # ),
  
  #============================================================
  
  fluidPage(title = "IPDGC Genome Browser",
            # activate javascript package
            useShinyjs(),
            #theme = shinytheme("cyborg"),
            includeCSS("www/theme.css"),
            fluidRow(
              column(width = 10,
                     # start hidden, show when search is performed
                     hidden(actionLink("wrapperlogo", img(src = "ipdgc_gb_small.png", id = "wrapperlogo")),
                            #actionLink("main", "Main"),
                            span(
                              actionLink("returnResults", "Return to results"), id = "mainPageLink")),
                     hidden(miniSearchBar, miniSubmitButton)),
              #hidden(span(miniSearchBar, miniSubmitButton, id = "minisubmit"))),
              column(width = 2,
                     #====Potential Dark Theme toggle button====
                     # dropdownButton(
                     #   materialSwitch(
                     #     inputId = "darktheme",
                     #     label = "Dark Theme",
                     #     value = F,
                     #     status = "primary"
                     #   ),
                     #   circle = F,
                     #   icon = icon("gear"),
                     #   size = "xs"),
                     #===========================================
                     div(actionLink("about", "About"), id = "top-row")),
              hr()),
            # This uiOutput allows rendering of UI inside this initial wrapper UI
            # When you want to render a UI inside the wrapper, define a "mainpage" variable
            # with renderUI() + whatever UI elements you want.
            uiOutput("mainPage"),
            tags$script(src = "clickdetect.js")
            
            #uiOutput("darktheme")
            
            #========Below is part II of sample Google login API==========
            # , div(id="signin", class="g-signin2", "data-onsuccess"="onSignIn"),
            # actionButton("signout", "Sign Out", onclick="signOut();", class="btn-danger"),
            # with(tags, dl(dt("Name"), dd(textOutput("g.name")),
            #               dt("Email"), dd(textOutput("g.email")),
            #               dt("Image"), dd(uiOutput("g.image")) ))
            #=============================================================
  )
)
#uiOutput("wrapUI")