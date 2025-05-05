library(shinydashboard)
library(bslib)
library(readxl)
library(DT)
library(igraph)
library(visNetwork)
library(datamods)
devtools::load_all("/home/vi/Desktop/TFG/fcaR-master")

games <- read.csv("~/Desktop/TFG/games.csv", row.names=1)
fca <- FormalContext$new(games)
set_objects <- Set$new(fca$objects)

server <- function(input, output, session) {

  # Evento para ir a get started
  observeEvent(input$btn_start, {
    updateTabItems(session, "tabs", "get_started")
  })

    # Para cargar dataset
    dataset <- import_file_server("file_dataset")


  # Generar botones objetos y atributos
    btn_objects <- fca$objects # esto seria un user input
    btn_attributes <- fca$attributes

    output$btn_objects <- renderUI({
      req(btn_objects)

      list(
      checkboxGroupInput("objects", "Objects:",
                         choiceNames = btn_objects,
                         choiceValues = btn_objects,
                         selected = 0
      ),
      verbatimTextOutput("att_in_common")
      )
    })

    output$btn_attributes <- renderUI({
      req(btn_attributes)

      checkboxGroupInput("attributes", "Attributes:",
                         choiceNames = btn_attributes,
                         choiceValues = btn_attributes
      )
    })

    output$att_in_common <- renderText({

      set_objects <- Set$new(fca$objects)
      sapply(input$objects, function(x){
        do.call(set_objects$assign, setNames(list(1), x))
        result <- fca$intent(set_objects)
        paste(capture.output(print(result)))
      })
    })

}
