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
    menuItem("Upload data", tabName = "upload_data", icon = icon("play")),
    menuItem("Basic operations", tabName = "basic_operations", icon = icon("play")),
    menuItem("Implications", tabName = "ui_implications", icon = icon("circle-info")),
    menuItem("Concepts", tabName = "ui_concepts", icon = icon("home")),
    menuItem("About us", tabName = "about-us", icon = icon("address-card"))
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
    #uiHome,
    uiUploadData,
    uiBasicOperations,
    uiImplications,
    tabItem(tabName = "ui_concepts",
            fluidRow(
              column(8,
                     box(width = 12,
                         title = "Plot",
                         status = "primary",
                         div(style = "height: 70vh; overflow-y: auto; align-items: center;",
                             visNetworkOutput("plot", height = "600px")
                         )
                     )
              ),
              column(4,
                     box(width = 12,
                         title = "Concept Summary",
                         status = "primary",
                         div(style = "height: 70vh; overflow-y: auto; align-items: center;",
                             verbatimTextOutput("conceptSelected"),
                             h6("Upper neighbours:"),
                             verbatimTextOutput("upperNeighbours"),
                             h6("Lower neighbours:"),
                             verbatimTextOutput("lowerNeighbours")
                         )
                     )
              )
            ),

            column(12,
                   div(style = "display: flex; align-items: center;",
                       div(
                         actionBttn("btnGoBackImpl", "Go back", icon = icon("arrow-left"),
                                    style = "unite", size = "sm", color = "primary")
                       ),
                       div(style = "margin-left: auto; display: flex; gap: 10px;",
                           actionBttn("createLatexConcepts", "Create table in LaTeX format", icon = icon("leaf"),
                                      style = "material-circle", size = "sm", color = "primary"),
                           downloadBttn("downloadRdsConp", "Download",
                                        style = "material-circle", size = "sm", color = "primary"),
                           #actionBttn("btnGoConcepts", "Go to concepts", icon = icon("arrow-right"),
                          #            style = "unite", size = "sm", color = "primary")
                       )
                   )
            )
    )

  )
)

ui <- dashboardPage(header, sidebar, body)
