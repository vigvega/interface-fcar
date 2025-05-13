uiBasicOperations <- tabItem(tabName = "basic_operations",
                             fluidRow(
                               column(6,
                                      box(width = 12,
                                          status = "primary",
                                          solidHeader = TRUE,
                                          title = "Formal Context",
                                          div(style = "height: 40vh; overflow-y: auto; whitespace:pre-wrap; ",
                                              verbatimTextOutput("formalContext"),
                                              actionButton("clarify", "Clarify"),
                                              actionButton("reduce", "Reduce")
                                          )
                                      ),
                                      box(width = 12,
                                          title = "Other actions",
                                          status = "primary",
                                          div(style = "height: 20vh; overflow-y: auto; align-items: center;",
                                              actionButton("createLatex", "Create table in LaTeX format", class = "btn btn-lg btn-block"),
                                              downloadButton("downloadRds", "Download", class = "btn btn-lg btn-block")
                                          )
                                      )
                               ),
                               column(6,
                                      box(width = 12,
                                          div(style="height: 76vh; whitespace:pre-wrap;",
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
                                    div(style = "display: flex; justify-content: space-between;",
                                        actionButton("btnGoUploadData", "", icon = icon("arrow-left")),
                                        actionButton("btnGoImplications", "", icon = icon("arrow-right"))
                                    )
                             )
)
