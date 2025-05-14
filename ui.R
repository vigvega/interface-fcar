library(shinydashboard)
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
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Upload data", tabName = "upload_data", icon = icon("play")),
    menuItem("Basic operations", tabName = "basic_operations", icon = icon("play")),
    menuItem("Implications", tabName = "ui_implications", icon = icon("circle-info")),
    menuItem("Concepts", tabName = "ui_concepts", icon = icon("home")),
    menuItem("About us", tabName = "about-us", icon = icon("address-card"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="style.css"),
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
    uiHome,
    uiUploadData,
    uiBasicOperations,
    tabItem(tabName = "ui_implications",
            fluidRow(
              column(12,
                     box(width = 12,
                         status = "primary",
                         solidHeader = TRUE,
                         title = "Implications",
                         div(style = "height: 40vh; overflow-y: auto; whitespace:pre-wrap; ",
                             column(6,
                             selectInput("selectLHS",
                                         "Choose attributes present in the left:",
                                         list("--Select--" = ""), multiple = TRUE)
                             ),
                             column(6,
                             selectInput("selectRHS",
                                         "Choose attributes present in the right:",
                                         list("--Select--" = ""), multiple = TRUE)
                             ),
                             column(12,
                             actionButton("btnApplyFilters", "Apply"),
                             input_task_button("btnClearFilters", "Clear"),
                             br(), br(),
                             verbatimTextOutput("fcImplications"),
                             input_task_button("simplifyImplications", "Simplify")
                             )
                         )
                     )
              )
            ),
            fluidRow(
              column(12,
                     box(width = 12,
                         title = "Closure",
                         status = "primary",
                         div(style = "height: 20vh; overflow-y: auto; align-items: center;",
                             selectInput("selectClosure",
                                         "Choose attributes to compute closure:",
                                         list("--Select--" = ""), multiple = TRUE),
                             verbatimTextOutput("implicationsClosure")
                            # p("Or get table for your LaTex doc..."),
                            # actionButton("createLatexImplications", "Create table in LaTeX format", class = "btn btn-lg btn-block")

                         )
                     )
              )

            ),

            column(12,
                   div(style = "display: flex; justify-content: space-between;",
                       actionButton("btnGoBasicOperations", "", icon = icon("arrow-left")),
                       actionButton("btnGoConcepts", "", icon = icon("arrow-right"))
                   )
            )
    )
    #uiConcepts
  )
)

ui <- dashboardPage(header, sidebar, body)
