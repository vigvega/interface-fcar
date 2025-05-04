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

      lapply(seq_along(btn_objects), function(i) {
        actionButton(inputId = paste0("btn_obj_", i), label = btn_objects[i])
      })
    })

    output$btn_attributes <- renderUI({
      req(btn_attributes)

      lapply(seq_along(btn_attributes), function(i) {
        actionButton(inputId = paste0("btn_attr_", i), label = btn_attributes[i])
      })
    })

    # Evento para btn_objects
    # Se muestran los atributos en común para todos los que sean pulsados
    observe({
      lapply(seq_along(btn_objects), function(i) {
        observeEvent(input[[paste0("btn_obj_", i)]], {
          output$clicked_button <- renderText({
            name <- btn_objects[i]
            set_objects$assign(name = 1)
            result <- fca$intent(set_objects)

            # Captura la impresión de consola como texto
            captured_output <- capture.output(print(result))
            paste(captured_output)
          })
        })
      })
    })

}
