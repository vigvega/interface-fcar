# Panel de implicaciones
conditionalPanel(
  condition = "input.select == 'i'",
  box(width = 8,
      status = "info",
      title = "Implications",
      div(style = "height: 50vh; overflow-y: auto; whitespace:pre-wrap; ",
          selectInput("selectRulesImplications",
                      "Choose rules to apply:",
                      list("Composition" = "composition", "Generalization" = "generalization", "Reduction" = "reduction", "Simplification" = "simplification"), multiple = TRUE),
          actionButton("btnApplyRules", "Apply"),
          actionButton("btnClearRules", "Clear"),
          verbatimTextOutput("fcImplications"),
          br(),
          actionButton("createLatexImplications", "Create table in LaTeX format", class = "btn btn-lg btn-block")
      )
  ),
  box(width = 4,
      status = "info",
      title = "Other operations",
      div(style = "height: 50vh; overflow-y: auto; whitespace:pre-wrap; ",
          selectInput("selectRulesFromImplications", "Choose rules to check if they hold in the formal context:", choices = NULL, multiple = TRUE),
          verbatimTextOutput("holdsIn")
      )
  )
) # Fin condicional para implicaciones
