uiBasicOperations <- tabItem(tabName = "basic_operations",
                             fluidRow(
                               column(6,
                                      box(width = 12,
                                          status = "primary",
                                          #solidHeader = TRUE,
                                          title = "Formal Context",
                                          #div(style = "height: 70vh; overflow: auto; whitespace:pre-wrap; ",
                                          #verbatimTextOutput("formalContext"),
                                          div(style = "height: 65vh; overflow-y: auto; whitespace:pre-wrap; ",
                                          uiOutput("fcBasicOperations"),
                                          ),
                                          actionBttn("clarify", "Clarify", style = "bordered", size = "sm", color = "primary"),
                                          actionBttn("reduce", "Reduce", style = "bordered", size = "sm", color = "primary")

                                      )
                               ),
                               column(6,
                                      box(width = 12,
                                          #       status = "primary",
                                          #      title = "Basic operations",
                                          div(style = "height: 75vh; overflow-y: auto; whitespace:pre-wrap; ",
                                              navset_card_tab(
                                                nav_panel("Intent",
                                                          br(),
                                                          box(width=12,
                                                              status="primary",
                                                              title="Choose objects to see which attributes they have in common",
                                                              solidHeader = TRUE,
                                                              multiInput(
                                                                inputId = "intentOptions",
                                                                label = "",
                                                                choices = list("None" = ""),
                                                                width = "100%")
                                                          ),
                                                          box(width = 12,
                                                              title = "Intent output",
                                                              status = "primary",
                                                              solidHeader = TRUE,
                                                              uiOutput("intentResult")
                                                          )
                                                ),
                                                nav_panel("Extent",
                                                          br(),
                                                          box(width=12,
                                                              status="primary",
                                                              title="Choose attributes to see which objects has those properties",
                                                              solidHeader = TRUE,
                                                              multiInput(
                                                                inputId = "extentOptions",
                                                                label = "",
                                                                choices = list("None" = ""),
                                                                width = "100%")
                                                          ),
                                                          box(width = 12,
                                                              title = "Extent output",
                                                              status = "primary",
                                                              solidHeader = TRUE,
                                                              uiOutput("extentResult")
                                                          )
                                                ),
                                                nav_panel("Closure",
                                                          br(),
                                                          box(width=12,
                                                              status="primary",
                                                              title="Choose attributes to see the smallest subset that they are in",
                                                              solidHeader = TRUE,
                                                              multiInput(
                                                                inputId = "closureOptions",
                                                                label = "",
                                                                choices = list("None" = ""),
                                                                width = "100%")
                                                          ),
                                                          box(width = 12,
                                                              title = "Closure output",
                                                              status = "primary",
                                                              solidHeader = TRUE,
                                                              uiOutput("closureResult")
                                                          )
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
