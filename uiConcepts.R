uiConcepts <- tabItem(tabName = "ui_concepts",
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
                             div(style = "display: flex; justify-content: space-between;",
                                 actionButton("btnGoBasicOperations", "", icon = icon("arrow-left")),
                                 actionButton("btnGoConcepts", "", icon = icon("arrow-right"))
                             )
                      )
)
