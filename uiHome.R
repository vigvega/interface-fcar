uiHome <- tabItem(tabName = "home",
                  fluidRow(
                    column(6, id="home-primero",
                           img(src='logo.png', class="logo")
                    ),
                    column(6, id="home-segundo",
                           h4("fcaR: Tools for Formal Concept Analysis", class="title"),
                           h1("Understand the core concepts of FCA and it's implementation in R", class="main-description"),
                           h5("The fcaR package provides data structures which allow the user to work seamlessly with formal contexts and sets of implications.",
                              class="secondary-description"),
                           br(),
                           div(class="div-btn",
                               actionButton("btn_start", "Get started", class="btn_start"),
                               actionButton("btn-github", "Github docs", class="btn-github", onclick="window.open('https://github.com/Malaga-FCA-group/fcaR', '_blank')")
                           )
                    )
                  )
)
