uiConcepts <- tabItem(tabName = "ui_concepts",
                      fluidRow(
                        column(6,
                               box(width = 12,
                                   status = "primary",
                                   solidHeader = TRUE,
                                   title = "Concepts",
                                   div(style = "height: 70vh; overflow-y: auto; whitespace:pre-wrap; ",
                                       verbatimTextOutput("fcConcepts"),
                                       actionButton("createLatexConcepts", "Create table in LaTeX format", class = "btn btn-lg btn-block")
                                   )
                               )
                        ),

                        column(6,
                               box(width = 12,
                                   title = "Plot",
                                   status = "primary",
                                   div(style = "height: 70vh; overflow-y: auto; align-items: center;",
                                       visNetworkOutput("plot", height = "600px")
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
