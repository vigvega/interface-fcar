library(shinydashboard)
library(bslib)
library(readxl)
library(DT)
library(igraph)
library(visNetwork)
library(datamods)
devtools::load_all("/home/vi/Desktop/TFG/fcaR-master")
# editbl

games <- read.csv("~/Desktop/TFG/games.csv", row.names=1)
fca <- FormalContext$new(games)

header <- dashboardHeader(title = "FACR PACKAGE")

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
                import_file_ui("file_dataset"),
              )
              )
            ),
            fluidRow(class = "box1",
              column(4,
                     box(width = 12,
                     selectInput(
                       "select",
                       "Select options below:",
                       list("Objects and Attributes" = "oa", "Concepts" = "c", "Implications" = "i")
                     )
                     )
              ),
              column(8,
                  conditionalPanel(
                    condition = "input.select == 'oa'",
                        box(status = "primary",
                            solidHeader = TRUE,
                            width = 4,
                            title = "Intent",
                            uiOutput("btn_objects")
                        ),
                        box(status = "primary",
                            solidHeader = TRUE,
                            width = 4,
                            title = "Extent",
                        uiOutput("btn_attributes")
                        ),
                        box(status = "primary",
                            solidHeader = TRUE,
                            width = 4,
                            title = "Closure",
                            uiOutput("btn_attributes2")
                        )
                    )


              )
            )
    )# Fin get started

  )
)

ui <- dashboardPage(header, sidebar, body)
