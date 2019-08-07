## UPLOAD COMMAND:
# library(rsconnect)
# deployApp(appFileManifest = "appManifest.txt", appName = "ExomeBrowser")

# This page sets up the global variables and the packages used in both ui.R and server.R
library(shiny)
library(data.table)
library(shinyjs) # shiny javascript package provides basic javascript functions
library(DT) # DT package provides interactive data table renders
library(shinyWidgets) # widgets
library(shinydashboard)
library(shinydashboardPlus)
library(shinyanimate)

'%ni%' <- Negate('%in%')

miniSearchBar <- searchInput(
  inputId = "minisearchBar", 
  label = "Enter your search terms:", 
  placeholder = "e.g. LRRK2, chr12, 12:40196744, 12:40000000-50000000, rs1422910994", 
  btnSearch = icon("search"), 
  btnReset = icon("remove"), 
  width = "50%"
)


#====initial data loading file
source("datatables.R", local = T)