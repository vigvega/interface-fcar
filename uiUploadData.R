library(shinyjs)
uiUploadData <- tabItem(tabName = "upload_data",
                        useShinyjs(),
                        fluidRow(
                          column(3,
                                 offset = 3,
                                 fileInput("file", "Upload your .csv, .rds or .cxt file", buttonLabel = "Upload...", accept = c(".csv", ".rds", ".cxt")),
                          ),
                          column(4,
                                 fluidRow(
                                   column(8,
                                          selectInput(
                                            "selectDataset",
                                            "Or... connect to fcarepository.org!",
                                            #list(choices = options)
                                            list("--Select--" = "")
                                          )
                                   ),
                                   column(4,
                                          br(),
                                          input_task_button("connectRepo", "Find datasets")
                                   )
                                 ),
                                 textOutput("connectionFailed")
                          ),
                        ),
                        fluidRow(
                          box(
                            width = 12,
                            title = "Data uploaded",
                            div(style = "height: 60vh; overflow: auto",
                                div(style="display: flex; justify-content: center; height: 60vh; overflow: auto;",
                                    div(style = "max-width: 90%; max-height: 90%; overflow: auto; display: inline-block;",
                                        uiOutput("contents")
                                    )
                                )
                            )
                          )
                        ),

                        fluidRow(
                          column(12,
                                 div(style = "text-align: right;",
                                     actionBttn("btnGoBasicOperations", "Go to basic operations", icon = icon("arrow-right"),
                                                style = "unite", size = "sm", color = "primary")
                                 )
                          )
                        )
)
