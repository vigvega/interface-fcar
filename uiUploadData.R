uiUploadData <- tabItem(tabName = "upload_data",
                        useShinyjs(),
                        box(
                          width = 12,
                          #collapsible = TRUE,
                          #collapsed = TRUE,
                          status = "info",
                          title = "Upload data",
                          div(style = "height: 70vh; overflow-y: auto;",
                              column(5,
                                     fileInput("file", "1. Select dataset (allowed formats: .csv, .txt)", buttonLabel = "Upload...", accept = c(".csv")),
                                     selectInput("delim", "Delimiter (optional)", list("--Select--" = "", "Comma" = ",", "Semicolon" = ";", "Whitespace" = " ", "Tab" = "\t")),
                                     numericInput("skip", "Skip rows",value = 0, min = 0, max = 100000),
                                     fileInput("fileRds", "2. Select .rds file", buttonLabel = "Upload...", accept = c(".rds")),
                                     selectInput(
                                       "selectDataset",
                                       "3. Or... use some of our preloaded data!",
                                       list("--Select--" = "", "Videogames" = "v", "Ganter" = "g", "Planets" = "p"),
                                       #selected = "v"
                                     )
                              ),
                              column(7,
                                     div(style = "height: 100%; overflow-y: auto;",
                                         uiOutput("contents")
                                         #DTOutput("contents")
                                     )
                              )
                          )
                        ),
                        column(12,
                               div(style = "text-align: right;",
                                   actionButton("btnGoBasicOperations", "", icon = icon("arrow-right"), disabled = TRUE)
                               )
                        )
)
