uiBasicOperations <- tabItem(tabName = "basic_operations",
                             fluidRow(
                               column(12,
                                      fluidRow(
                                        column(6,
                                               box(width = 12,
                                                   status = "primary",
                                                   solidHeader = TRUE,
                                                   title = "Formal Context",
                                                   div(style = "height: 40vh; overflow-y: auto; whitespace:pre-wrap; ",
                                                       verbatimTextOutput("formalContext")
                                                   )
                                               )
                                        ),

                                        column(2,
                                               box(width=12,
                                                   status = "primary",
                                                   solidHeader = TRUE,
                                                   title = "Extent",
                                                   div(style = "height: 40vh; overflow-y: auto;",
                                                       uiOutput("btn_objects")
                                                   )
                                               )
                                        ),
                                        column(2,
                                               box(width=12,
                                                   status = "primary",
                                                   solidHeader = TRUE,
                                                   title = "Intent",
                                                   div(style = "height: 40vh; overflow-y: auto;",
                                                       uiOutput("btn_attributes")
                                                   )
                                               )
                                        ),
                                        column(2,
                                               box(width=12,
                                                   status = "primary",
                                                   solidHeader = TRUE,
                                                   title = "Closure",
                                                   div(style = "height: 40vh; overflow-y: auto;",
                                                       uiOutput("btn_attributes2")
                                                   )
                                               )


                                        ),

                                      )
                               ),
                               fluidRow(
                                 column(12,
                                        column(6,
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
                                                   title = "Console output",
                                                   status = "primary",
                                                   div(style = "height: 20vh; overflow-y: auto;",
                                                       verbatimTextOutput("intent"),
                                                       verbatimTextOutput("extent"),
                                                       verbatimTextOutput("closure")
                                                   )
                                               )
                                        )
                                 )
                               )
                             ),

                             column(12,
                                    div(style = "display: flex; justify-content: space-between;",
                                        actionButton("btnGoUploadData", "", icon = icon("arrow-left")),
                                        actionButton("btnGoImplications", "", icon = icon("arrow-right"))
                                    )
                             )
)
