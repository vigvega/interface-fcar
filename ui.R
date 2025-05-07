library(shinydashboard)
library(bslib)
library(readxl)
library(DT)
library(igraph)
library(visNetwork)
library(datamods)
library(fcaR)
#devtools::load_all("/home/vi/Desktop/TFG/fcaR-master")
# editbl

header <- dashboardHeader(title = "FCAR PACKAGE")

sidebar <- dashboardSidebar(
  collapsed = TRUE, # la barra lateral la oculto por defecto
  sidebarMenu(
    id="tabs",
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Get started", tabName = "get_started", icon = icon("play")),
    menuItem("More about FCAR", tabName = "info", icon = icon("circle-info")),
    menuItem("About us", tabName = "about-us", icon = icon("address-card"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="style.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Lora:wght@400;700&display=swap")
  ),
  tabItems(
    # Home
    tabItem(tabName = "home",
      fluidRow(
        column(6, id="home-primero",
               img(src='logo.png', class="logo")
        ),
        column(6, id="home-segundo",
          h4("fcaR: Tools for Formal Concept Analysis", class="title"),
          h1("Understand the core concepts of FCA and it's implementation in R", class="main-description"),
          h5("The fcaR package provides data structures which allow the user to work seamlessly with formal contexts and sets of implications.",
             class="secondary-description"),
          br(),
          div(class="div-btn",
          actionButton("btn_start", "Get started", class="btn_start"),
          actionButton("btn-github", "Github docs", class="btn-github", onclick="window.open('https://github.com/Malaga-FCA-group/fcaR', '_blank')")
          )
      )
      )
    ), # Fin home

    # Get started
    tabItem(tabName = "get_started",
            fluidRow(
              column(12,
              box(
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                status = "info",
                title = "Upload data",
                column(4,
                fileInput("file", "Data (allowed formats: .csv, .txt)", buttonLabel = "Upload...", accept = c(".csv")),
                textInput("delim", "Delimiter (optional)", ""),
                selectInput(
                  "selectDataset",
                  "Or... use some of our preloaded data!",
                  list("--Select--" = "", "Videogames" = "v", "Ganter" = "g", "Planets" = "p")
                ),
                ),
                column(8,
                       div(style = "height: 100%; overflow-y: auto;",
                       DTOutput("contents"))
                )

              )
              )
            ),
            fluidRow(class = "box1",
              column(3,
                     box(width = 12,
                         status = "info",
                         title = "Select options below",
                     selectInput(
                       "select",
                       "",
                       list("Basic operations" = "oa", "Concepts" = "c", "Implications" = "i")
                     )
                     )
              ),
              column(9,
                     # Panel de operaciones básicas
                  conditionalPanel(
                    condition = "input.select == 'oa'",
                        box(width = 8,
                            status = "info",
                            title = "Formal Context",
                            div(style = "height: 50vh; overflow-y: auto; whitespace:pre-wrap; ",
                            verbatimTextOutput("formalContext"),
                            actionButton("createLatex", "Create table in LaTeX format", class = "btn btn-lg btn-block"),
                            downloadButton("downloadRds", "Download", class = "btn btn-lg btn-block")

                            )
                        ),
                        box(width = 4,
                            status = "info",
                            title = "Operations",
                            div(style = "height: 50vh; overflow-y: auto;",
                            selectInput(
                              "selectOperation",
                              "",
                              list("Extent" = "ex", "Intent" = "in", "Closure" = "cl")
                                ),
                            conditionalPanel(
                              condition = "input.selectOperation == 'in'",
                              uiOutput("btn_objects"),
                            ),
                            conditionalPanel(
                              condition = "input.selectOperation == 'ex'",
                              uiOutput("btn_attributes"),
                            ),
                            conditionalPanel(
                              condition = "input.selectOperation == 'cl'",
                              uiOutput("btn_attributes2"),
                            )
                            )
                        ),
                    column(12,
                    fluidRow(
                      box(status="info",
                          width = 12,
                          conditionalPanel(
                            condition = "input.selectOperation == 'in'",
                            verbatimTextOutput("intent")
                          ),
                          conditionalPanel(
                            condition = "input.selectOperation == 'ex'",
                            verbatimTextOutput("extent")
                          ),
                          conditionalPanel(
                            condition = "input.selectOperation == 'cl'",
                            verbatimTextOutput("closure")
                          ))
                      )
                    )
                    ), # Fin condicional para operaciones básicas
                  # Panel de implicaciones
                  conditionalPanel(
                    condition = "input.select == 'i'",
                    box(width = 8,
                        status = "info",
                        title = "Implications",
                        div(style = "height: 50vh; overflow-y: auto; whitespace:pre-wrap; ",
                            selectInput("selectRulesImplications",
                                        "Choose rules to apply:",
                                        list("Composition" = "composition", "Generalization" = "generalization", "Reduction" = "reduction", "Simplification" = "simplification"), multiple = TRUE),
                            actionButton("btnApplyRules", "Apply"),
                            actionButton("btnClearRules", "Clear"),
                            verbatimTextOutput("fcImplications"),
                            br(),
                            actionButton("createLatexImplications", "Create table in LaTeX format", class = "btn btn-lg btn-block")
                        )
                    ),
                    box(width = 4,
                        status = "info",
                        title = "Other operations",
                        div(style = "height: 50vh; overflow-y: auto; whitespace:pre-wrap; ",
                            selectInput("selectRulesFromImplications", "Choose rules to check if they hold in the formal context:", choices = NULL, multiple = TRUE),
                            verbatimTextOutput("holdsIn")
                            )
                    )
                  ) # Fin condicional para implicaciones
              )
            )
    )# Fin get started

  )
)

ui <- dashboardPage(header, sidebar, body)
