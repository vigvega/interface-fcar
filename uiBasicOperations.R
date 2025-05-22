uiBasicOperations <-     tabItem(tabName = "basic_operations",
                                 fluidRow(
                                   column(6,
                                          box(width = 12,
                                              status = "primary",
                                              #solidHeader = TRUE,
                                              title = "Formal Context",
                                              #div(style = "height: 70vh; overflow-y: auto; whitespace:pre-wrap; ",
                                              verbatimTextOutput("formalContext"),
                                              actionBttn("clarify", "Clarify", style = "bordered", size = "sm", color = "primary"),
                                              actionBttn("reduce", "Reduce", style = "bordered", size = "sm", color = "primary")
                                              #)
                                          )
                                   ),
                                   column(6,
                                          box(width = 12,
                                              status = "primary",
                                              title = "Basic operations",
                                              div(style = "height: 70vh; overflow-y: auto; whitespace:pre-wrap; ",
                                                  navset_card_tab(
                                                    nav_panel("Intent",
                                                              multiInput(
                                                                inputId = "intentOptions",
                                                                label = "Choose objects to see which attributes they have in common",
                                                                choices = list("None" = ""),
                                                                width = "100%"),
                                                              verbatimTextOutput("intentResult")
                                                    ),
                                                    nav_panel("Extent",
                                                              multiInput(
                                                                inputId = "extentOptions",
                                                                label = "Choose attributes to see which objects has those properties",
                                                                choices = list("None" = ""),
                                                                width = "100%"),
                                                              verbatimTextOutput("extentResult")
                                                    ),
                                                    nav_panel("Closure",
                                                              multiInput(
                                                                inputId = "closureOptions",
                                                                label = "Choose attributes to see the smallest subset that they are in",
                                                                choices = list("None" = ""),
                                                                width = "100%"),
                                                              verbatimTextOutput("closureResult")
                                                    )
                                                  )
                                              )
                                          )
                                   ),
                                 ),
                                 column(12,
                                        div(style = "display: flex; align-items: center;",
                                            div(
                                              actionBttn("btnGoUploadData", "Go back", icon = icon("arrow-left"),
                                                         style = "unite", size = "sm", color = "primary")
                                            ),
                                            div(style = "margin-left: auto; display: flex; gap: 10px;",
                                                actionBttn("createLatex", "Create table in LaTeX format", icon = icon("leaf"),
                                                           style = "material-circle", size = "sm", color = "primary"),
                                                downloadBttn("downloadRds", "Download",
                                                             style = "material-circle", size = "sm", color = "primary"),
                                                actionBttn("btnGoConcepts", "Go to concepts", icon = icon("arrow-right"),
                                                           style = "unite", size = "sm", color = "primary")
                                            )
                                        )

                                 )
)
