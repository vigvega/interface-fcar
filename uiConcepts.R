uiConcepts <-  tabItem(tabName = "ui_concepts",
                       useShinyjs(),
                       box(width = 12,
                           navset_card_tab(
                             nav_panel("Autonomous Exploration",
                                       fluidRow(
                                         column(7,
                                                div(style = "height: 60vh;  align-items: center;",
                                                    visNetworkOutput("plot", height = "520px")
                                                )
                                         ),
                                         column(5,
                                                div(style = "height: 60vh; overflow-y: auto; align-items: center;",
                                                    br(),
                                                    box(width = 12,
                                                        title = "Concept selected",
                                                        status = "primary",
                                                        solidHeader = TRUE,
                                                        uiOutput("conceptSelected")
                                                    ),
                                                    box(width = 12,
                                                        title = "Upper neighbours",
                                                        status = "primary",
                                                        solidHeader = TRUE,
                                                        uiOutput("upperNeighbours")
                                                    ),
                                                    box(width = 12,
                                                        title = "Lower neighbours",
                                                        status = "primary",
                                                        solidHeader = TRUE,
                                                        uiOutput("lowerNeighbours")
                                                    )
                                                )
                                         )
                                       )
                             ),
                             nav_panel("Guided exploration",
                                       fluidRow(
                                         column(7,
                                                fluidRow(
                                                  column(5,
                                                         selectInput(
                                                           inputId = "initAtt",
                                                           label = "Choose initial attributes",
                                                           choices = list("None" = ""),
                                                           multiple = TRUE)
                                                  ),
                                                  column(5,
                                                         selectInput(
                                                           inputId = "targetAtt",
                                                           label = "Choose target attributes",
                                                           choices = list("None" = ""),
                                                           multiple = TRUE)
                                                  ),
                                                  column(2,
                                                         fluidRow(
                                                           br(),
                                                           actionBttn("btnBeginExploration", "", icon = icon("magnifying-glass"),
                                                                      style = "unite", size = "sm", color = "primary"),
                                                           actionBttn("btnClearExploration", "", icon = icon("broom"),
                                                                      style = "unite", size = "sm", color = "primary"))
                                                  )
                                                ),
                                                fluidRow(
                                                  column(12,
                                                         div(style = "height: 60vh;  align-items: center;",
                                                             visNetworkOutput("plot2", height = "400px"),
                                                             div(style = "text-align: right;",
                                                                 hidden(
                                                                   actionBttn("btnNextStep", "Next step", style = "jelly", size = "sm", color = "warning")
                                                                 )
                                                             )
                                                         )
                                                  )
                                                )
                                         ),
                                         column(5,
                                                br(),
                                                navset_card_tab(
                                                  nav_panel("Concepts",
                                                            div(style = "height: 60vh; overflow: auto",
                                                                br(),
                                                                tableOutput("allConcepts")
                                                            )
                                                  ),
                                                  nav_panel("Selection",
                                                            br(),
                                                            div(style = "height: 60vh; overflow-y: auto; align-items: center;",
                                                                box(width = 12,
                                                                    title = "Initial concept",
                                                                    status = "primary",
                                                                    solidHeader = TRUE,
                                                                    uiOutput("initConcept")
                                                                ),
                                                                box(width = 12,
                                                                    title = "Target concept",
                                                                    status = "primary",
                                                                    solidHeader = TRUE,
                                                                    uiOutput("targetConcept")
                                                                ),
                                                                box(width = 12,
                                                                    title = "Next step",
                                                                    status = "primary",
                                                                    solidHeader = TRUE,
                                                                    uiOutput("nextStepConcepts")
                                                                )
                                                            )
                                                  ),
                                                  nav_panel("Exploration history",
                                                            br(),
                                                            div(style = "height: 60vh; overflow-y: auto; align-items: center;",
                                                                uiOutput("history")
                                                            )
                                                  )
                                                )
                                         )
                                       )
                             )
                           )
                       ),

                       column(12,
                              div(style = "display: flex; align-items: center;",
                                  div(
                                    actionBttn("btnGoBack", "Go back", icon = icon("arrow-left"),
                                               style = "unite", size = "sm", color = "primary")
                                  ),
                                  div(style = "margin-left: auto; display: flex; gap: 10px;",
                                      actionBttn("createLatexConcepts", "Create table in LaTeX format", icon = icon("leaf"),
                                                 style = "material-circle", size = "sm", color = "primary"),
                                      downloadBttn("downloadRdsConp", "Download",
                                                   style = "material-circle", size = "sm", color = "primary"),
                                      actionBttn("btnGoImplications", "Go to implications", icon = icon("arrow-right"),
                                                 style = "unite", size = "sm", color = "primary")
                                  )
                              )
                       )
)
