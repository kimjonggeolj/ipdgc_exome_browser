# This page sets up the global variables and the packages used in both ui.R and server.R
library(shiny)
library(data.table)
library(shinyjs) # shiny javascript package provides basic javascript functions
library(DT) # DT package provides interactive data table renders
library(shinyWidgets)
library(shinythemes) # may be used to add dark theme functionality
library(shinydashboard)
library(shinydashboardPlus)
library(shinyanimate)
# enable bookmarking
# enableBookmarking(store = "url")
# 1 = initial main page
# 2 = result page
# 3 = gene page
# 4 = variant page

#boxWidth <- 12
'%ni%' <- Negate('%in%')

miniSearchBar <- searchInput(
  inputId = "minisearchBar", 
  label = "Enter your search terms:", 
  placeholder = "e.g. LRRK2, chr12, 12:40196744, 12:40000000-50000000, rs1422910994", 
  btnSearch = icon("search"), 
  btnReset = icon("remove"), 
  width = "50%"
)
  # span(textInput("minisearchBar",
  #                              label = NULL,
  #                              placeholder = "e.g. LRRK2, chr12, 12:40000000-50000000, rs1422910994",
  #                              width = "400px"), id= "miniSearchBar", style = "display:inline-block;vertical-align:top;margin-top:2em")

#miniSubmitButton <- span(actionButton("minisubmit", "Search"), onclick = '"aboutClick"', id = "minisubmit", style = "display:inline-block;vertical-align:top;margin-top:2em")


#====initial data loading file
source("datatables.R", local = T)