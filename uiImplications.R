uiImplications <- tabItem(tabName = "ui_implications",
                          fluidRow(
                            column(12,
                                   box(width = 12,
                                       status = "primary",
                                       solidHeader = TRUE,
                                       title = "Implications",
                                       div(style = "height: 40vh; overflow-y: auto; whitespace:pre-wrap; ",
                                           selectInput("selectRulesImplications",
                                                       "Choose rules to apply:",
                                                       list("Composition" = "composition", "Generalization" = "generalization", "Reduction" = "reduction", "Simplification" = "simplification"), multiple = TRUE),
                                           actionButton("btnApplyRules", "Apply"),
                                           actionButton("btnClearRules", "Clear"),
                                           br(), br(),
                                           verbatimTextOutput("fcImplications"),
                                       )
                                   )
                            )
                          ),
                          fluidRow(
                            column(12,
                                   box(width = 12,
                                       title = "Other actions",
                                       status = "primary",
                                       div(style = "height: 20vh; overflow-y: auto; align-items: center;",
                                           selectInput("selectRulesFromImplications", "Choose rules to check if they hold in the formal context", list("--Select--" = ""), multiple = TRUE),
                                           verbatimTextOutput("holdsIn"),
                                           p("Or get table for your LaTex doc..."),
                                           actionButton("createLatexImplications", "Create table in LaTeX format", class = "btn btn-lg btn-block")

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
