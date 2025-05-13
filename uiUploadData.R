library(shinyjs)
uiUploadData <- tabItem(tabName = "upload_data",
                        useShinyjs(),
                        fluidRow(
                        box(
                          width = 12,
                          status = "info",
                          title = "Upload data",
                          div(style = "height: 75vh; overflow: auto",
                              fluidRow(
                                column(6,
                                       fileInput("file", "Upload your .csv, .rds or .cxt file", buttonLabel = "Upload...", accept = c(".csv", ".rds", ".cxt")),
                                ),
                                column(6,
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
                                hr(),
                                div(style="display: flex; justify-content: center; height: 60vh; overflow: auto;",
                                    div(style = "max-width: 100%; max-height: 100%; overflow: auto; display: inline-block;",
                                        uiOutput("contents")
                                    )
                                )
                              )
                          )
                        )
                        ),
                        fluidRow(
                          column(12,
                                 div(style = "text-align: right;",
                                     actionButton("btnGoBasicOperations", "", icon = icon("arrow-right"), disabled = TRUE)
                                 )
                          )
                        )
)
