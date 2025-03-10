    tabItem(tabName = "get_started",
            fluidRow(
              box(
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                status = "info",
                title = "Upload data",
                import_ui("file_dataset"),
              )
            ),
            fluidRow(
              box(
                width = 12,
                collapsible = TRUE,
                status = "primary",
                solidHeader = TRUE,
                title = "Formal Context",
                verbatimTextOutput("code_fc"),
                verbatimTextOutput("fc")
              )
            ),
            fluidRow(
              box(
                width = 4,
                collapsible = TRUE,
                status = "danger",
                solidHeader = TRUE,
                title = "Set of concepts",
                verbatimTextOutput("code_concepts"),
                verbatimTextOutput("concepts")
              ),
              box(
                width = 4,
                collapsible = TRUE,
                status = "success",
                solidHeader = TRUE,
                title = "Set of implications",
                verbatimTextOutput("code_implications"),
                verbatimTextOutput("implications")
              ),
            box(
              title = "Plot the concept lattice",
              width = 12,
              collapsible = TRUE,
              status = "warning",
              solidHeader = TRUE,
              conditionalPanel(
                condition = "output.datasetNull",
                imageOutput("img_no_data")
              ),
              conditionalPanel(
                condition = "!output.datasetNull",
                plotOutput("graphFormalConcept")
              )
            )
  )
)# Fin get started

