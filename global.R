## UPLOAD COMMAND:
# library(rsconnect)
# deployApp(appFileManifest = "appManifest.txt", appName = "ExomeBrowser", account = 'pdgenetics')

# This page sets up the global variables and the packages used in both ui.R and server.R
library(shiny)
library(data.table)
library(shinyjs) # shiny javascript package provides basic javascript functions
library(DT) # DT package provides interactive data table renders
library(shinyWidgets) # widgets
library(shinydashboard)
library(shinydashboardPlus)
library(shinyanimate)
library(plotly)
library(ggplot2)
library(stringr)
# library(pander)
# library(markdown)
# library(reshape2)

'%ni%' <- Negate('%in%')

runfromPlotly <- F

#====initial data loading file
source("datatables.R", local = T)