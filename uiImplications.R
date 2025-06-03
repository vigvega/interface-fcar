uiImplications <- tabItem(tabName = "ui_implications",
                          fluidRow(
                            column(6,
                                   box(width = 12,
                                       collapsible = TRUE,
                                       #collapsed = TRUE,
                                       title = "Filters",
                                       column(6,
                                              selectInput("selectLHS",
                                                          "Attributes in the left:",
                                                          list("--Select--" = ""), multiple = TRUE),
                                              selectInput("selectNotLHS",
                                                          "Attributes NOT in the left:",
                                                          list("--Select--" = ""), multiple = TRUE),
                                       ),
                                       column(6,
                                              selectInput("selectRHS",
                                                          "Attributes in the right:",
                                                          list("--Select--" = ""), multiple = TRUE),
                                              selectInput("selectNotRHS",
                                                          "Attributes NOT in the right:",
                                                          list("--Select--" = ""), multiple = TRUE)

                                       ),
                                       sliderInput("support", "Support greater than:", 0, 1.0, 0, step = 0.1),
                                       actionBttn("btnApplyFilters", "Apply", style = "bordered", size = "sm", color = "primary"),
                                       actionBttn("btnClearFilters", "Clear", style = "bordered", size = "sm", color = "primary")
                                   ),
                            ),
                            column(6,
                                   box(width = 12,
                                       title = "Implications",
                                       status = "primary",
                                       div(style = "height: 55vh; overflow-y: auto; ",
                                           selectInput("selectRulesImplications",
                                                       "Choose rules to apply:",
                                                       list(
                                                         "Composition" = "composition",
                                                         "Generalization" = "generalization",
                                                         "Reduction" = "reduction",
                                                         "Simplification" = "simplification"),
                                                       multiple = TRUE),
                                           uiOutput("fcImplications")
                                       ),
                                       selectInput("selectClosure",
                                                   "Choose attributes to compute closure:",
                                                   list("--Select--" = ""), multiple = TRUE),
                                       textOutput("implicationsClosure")
                                   )
                            )

                          ),

                          column(12,
                                 div(style = "display: flex; align-items: center;",
                                     div(
                                       actionBttn("btnGoBackConcep", "Go back", icon = icon("arrow-left"),
                                                  style = "unite", size = "sm", color = "primary")
                                     ),
                                     div(style = "margin-left: auto; display: flex; gap: 10px;",
                                         actionBttn("createLatexImplications", "Create table in LaTeX format", icon = icon("leaf"),
                                                    style = "material-circle", size = "sm", color = "primary"),
                                         downloadBttn("downloadRdsImp", "Download",
                                                      style = "material-circle", size = "sm", color = "primary")
                                     )
                                 )
                          )
)
