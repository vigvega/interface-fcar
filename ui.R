library(shinydashboard)
library(shiny)
library(bslib)
library(readxl)
library(DT)
library(igraph)
library(visNetwork)
library(datamods)
library(fcaR)
library(shinyjs)
library(shinyWidgets)
#devtools::load_all("/home/vi/Desktop/TFG/fcaR-master")

source("uiHome.R")
source("uiUploadData.R")
source("uiBasicOperations.R")
source("uiImplications.R")
source("uiConcepts.R")
# editbl

header <- dashboardHeader(title = "FCAR PACKAGE")

sidebar <- dashboardSidebar(
  collapsed = TRUE, # la barra lateral la oculto por defecto
  sidebarMenu(
    id="tabs",
    #menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Upload data", tabName = "upload_data", icon = icon("file")),
    menuItem("Basic operations", tabName = "basic_operations", icon = icon("play")),
    menuItem("Concepts", tabName = "ui_concepts", icon = icon("object-ungroup")),
    menuItem("Implications", tabName = "ui_implications", icon = icon("circle-info"))
  )
)

body <- dashboardBody(
  tags$head(
    #tags$link(rel="stylesheet", type="text/css", href="style.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Lora:wght@400;700&display=swap"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto+Mono&display=swap"),
    tags$style(HTML("
      .roboto-mono {
        font-family: 'Roboto Mono', monospace;
      }

      #closure, #intent, #extent {
        max-height: none !important;
        overflow-y: visible !important;
        white-space: pre-wrap;
      }
    "))
  ),
  tabItems(
    uiUploadData,
    uiBasicOperations,
    uiImplications,
    uiConcepts
  )
)

ui <- dashboardPage(header, sidebar, body)
